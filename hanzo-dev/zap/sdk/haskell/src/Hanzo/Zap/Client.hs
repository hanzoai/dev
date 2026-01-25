{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Hanzo.Zap.Client
-- Copyright   : (c) 2025 Hanzo AI Inc.
-- License     : MIT
-- Maintainer  : dev@hanzo.ai
-- Stability   : experimental
-- Portability : portable
--
-- ZAP client implementation for connecting to ZAP servers.
--
-- = Quick Start
--
-- @
-- import Hanzo.Zap.Client
-- import Data.Aeson (object, (.=))
--
-- main :: IO ()
-- main = do
--     client <- connect "zap://localhost:9999"
--     result <- callTool client "read_file" $ object ["path" .= "src/main.rs"]
--     print result
--     disconnect client
-- @
--
-- = With Resource Management
--
-- @
-- import Hanzo.Zap.Client
-- import Control.Exception (bracket)
--
-- main :: IO ()
-- main = withClient "zap://localhost:9999" $ \\client -> do
--     result <- callTool client "git_status" $ object []
--     print result
-- @

module Hanzo.Zap.Client
    ( -- * Client Type
      ZapClient
    , ClientConfig (..)
    , defaultClientConfig

      -- * Connection
    , connect
    , connectWithConfig
    , disconnect
    , withClient
    , withClientConfig

      -- * Tool Calls
    , callTool
    , callToolWithTimeout
    , listTools
    , batchCall

      -- * Low-level
    , sendRequest
    , recvResponse
    , ping

      -- * State
    , isConnected
    , getCapabilities
    , getSessionId
    ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_, withMVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Exception (bracket, IOException, try)
import Control.Monad (when)
import Data.Aeson (Value (..), encode, decode', object, (.=))
import Data.Aeson.KeyMap (toMapText)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import GHC.Generics (Generic)
import Network.Socket
    ( Socket
    , AddrInfo (..)
    , SocketType (..)
    , getAddrInfo
    , socket
    , connect'
    , close
    , defaultHints
    )
import Network.Socket.ByteString (recv, sendAll)

import Hanzo.Zap.Types

-------------------------------------------------------------------------------
-- Client Configuration
-------------------------------------------------------------------------------

-- | Client configuration options.
data ClientConfig = ClientConfig
    { cfgName           :: Text
    , cfgVersion        :: Text
    , cfgConnectTimeout :: Int  -- ^ Milliseconds
    , cfgRequestTimeout :: Int  -- ^ Milliseconds
    , cfgRetryAttempts  :: Int
    , cfgRetryDelayMs   :: Int
    }
    deriving stock (Eq, Show, Generic)

-- | Default client configuration.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
    { cfgName           = "hanzo-zap-haskell"
    , cfgVersion        = "0.6.0"
    , cfgConnectTimeout = 10000
    , cfgRequestTimeout = 30000
    , cfgRetryAttempts  = 3
    , cfgRetryDelayMs   = 100
    }

-------------------------------------------------------------------------------
-- Client State
-------------------------------------------------------------------------------

-- | Internal client state.
data ClientState = ClientState
    { stSocket       :: Socket
    , stCapabilities :: Capabilities
    , stSessionId    :: Text
    , stNextId       :: IORef Word32
    , stPending      :: TVar (Map RequestId (MVar Response))
    , stConnected    :: TVar Bool
    }

-- | ZAP client handle.
--
-- Thread-safe handle for communicating with a ZAP server.
-- Use 'connect' or 'withClient' to create.
newtype ZapClient = ZapClient (MVar ClientState)

-------------------------------------------------------------------------------
-- Connection
-------------------------------------------------------------------------------

-- | Parse a ZAP URI into host and port.
parseUri :: String -> Either String (String, Int)
parseUri uri = case T.stripPrefix "zap://" (T.pack uri) of
    Nothing -> case T.stripPrefix "zaps://" (T.pack uri) of
        Nothing -> Left "URI must start with zap:// or zaps://"
        Just rest -> parseHostPort rest 9999
    Just rest -> parseHostPort rest 9999
  where
    parseHostPort t defaultPort =
        let (h, p) = T.breakOn ":" t
        in if T.null p
           then Right (T.unpack h, defaultPort)
           else case reads (T.unpack $ T.drop 1 p) of
               [(port, "")] -> Right (T.unpack h, port)
               _            -> Left "Invalid port number"

-- | Connect to a ZAP server.
--
-- @
-- client <- connect "zap://localhost:9999"
-- @
connect :: String -> IO ZapClient
connect = connectWithConfig defaultClientConfig

-- | Connect to a ZAP server with custom configuration.
connectWithConfig :: ClientConfig -> String -> IO ZapClient
connectWithConfig cfg uri = do
    (host, port) <- case parseUri uri of
        Left err -> fail err
        Right hp -> pure hp

    -- Resolve address
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    case addrs of
        [] -> fail $ "Cannot resolve: " <> host
        (addr:_) -> do
            -- Create socket
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect' sock (addrAddress addr)

            -- Initialize state
            nextId <- newIORef 1
            pending <- newTVarIO Map.empty
            connected <- newTVarIO True

            let state = ClientState
                    { stSocket       = sock
                    , stCapabilities = Capabilities False False False False
                    , stSessionId    = ""
                    , stNextId       = nextId
                    , stPending      = pending
                    , stConnected    = connected
                    }

            stateMVar <- newMVar state

            -- Perform handshake
            let client = ZapClient stateMVar
            caps <- performHandshake client cfg

            -- Update capabilities
            modifyMVar_ stateMVar $ \s -> pure s { stCapabilities = caps }

            pure client

-- | Perform the ZAP handshake.
performHandshake :: ZapClient -> ClientConfig -> IO Capabilities
performHandshake (ZapClient stateMVar) cfg = withMVar stateMVar $ \state -> do
    let initMsg = InitMessage
            { initName    = cfgName cfg
            , initVersion = cfgVersion cfg
            }

    -- Send Init message
    let payload = BL.toStrict $ encode initMsg
    sendMessage (stSocket state) 0x01 payload

    -- Receive InitAck
    (msgType, respPayload) <- recvMessage (stSocket state)
    when (msgType /= 0x02) $
        fail $ "Expected InitAck (0x02), got: " <> show msgType

    case decode' (BL.fromStrict respPayload) of
        Nothing  -> fail "Failed to parse InitAck"
        Just ack -> pure (ackCapabilities ack)

-- | Disconnect from the server.
disconnect :: ZapClient -> IO ()
disconnect (ZapClient stateMVar) = modifyMVar_ stateMVar $ \state -> do
    atomically $ writeTVar (stConnected state) False
    close (stSocket state)
    pure state

-- | Execute an action with a ZAP client, ensuring cleanup.
--
-- @
-- withClient "zap://localhost:9999" $ \\client -> do
--     result <- callTool client "git_status" $ object []
--     print result
-- @
withClient :: String -> (ZapClient -> IO a) -> IO a
withClient = withClientConfig defaultClientConfig

-- | Execute an action with a configured ZAP client.
withClientConfig :: ClientConfig -> String -> (ZapClient -> IO a) -> IO a
withClientConfig cfg uri = bracket (connectWithConfig cfg uri) disconnect

-------------------------------------------------------------------------------
-- Tool Calls
-------------------------------------------------------------------------------

-- | Call a tool on the ZAP server.
--
-- @
-- result <- callTool client "read_file" $ object ["path" .= "src/main.rs"]
-- @
callTool :: ZapClient -> Text -> Value -> IO (Either ZapError Value)
callTool client name args = callToolWithTimeout client name args 30000

-- | Call a tool with a specific timeout.
callToolWithTimeout :: ZapClient -> Text -> Value -> Int -> IO (Either ZapError Value)
callToolWithTimeout client@(ZapClient stateMVar) name args timeoutMs = do
    reqId <- withMVar stateMVar $ \state -> do
        nextId <- atomicModifyIORef' (stNextId state) $ \n -> (n + 1, n)
        pure $ "req-" <> T.pack (show nextId)

    let req = Request
            { reqId        = reqId
            , reqToolName  = name
            , reqArguments = args
            , reqMetadata  = Nothing
            }

    -- Send request
    sendRequest client req

    -- Wait for response with timeout
    result <- race
        (threadDelay (timeoutMs * 1000))
        (recvResponse client reqId)

    case result of
        Left () -> pure $ Left ZapError
            { errCode    = ErrTimeout
            , errMessage = "Request timed out"
            , errData    = Nothing
            }
        Right resp -> case respError resp of
            Just err -> pure $ Left err
            Nothing  -> pure $ Right $ maybe Null id (respContent resp)

-- | List available tools on the server.
listTools :: ZapClient -> IO (Either ZapError [Tool])
listTools (ZapClient stateMVar) = withMVar stateMVar $ \state -> do
    -- Send ListTools message
    sendMessage (stSocket state) 0x10 BS.empty

    -- Receive ToolList
    (msgType, payload) <- recvMessage (stSocket state)
    case msgType of
        0x11 -> case decode' (BL.fromStrict payload) of
            Just tools -> pure $ Right tools
            Nothing    -> pure $ Left ZapError
                { errCode    = ErrParseError
                , errMessage = "Failed to parse tool list"
                , errData    = Nothing
                }
        0xFE -> case decode' (BL.fromStrict payload) of
            Just err -> pure $ Left err
            Nothing  -> pure $ Left ZapError
                { errCode    = ErrInternalError
                , errMessage = "Failed to parse error"
                , errData    = Nothing
                }
        _ -> pure $ Left ZapError
            { errCode    = ErrInternalError
            , errMessage = "Unexpected message type"
            , errData    = Nothing
            }

-- | Execute multiple tool calls in a batch.
batchCall :: ZapClient -> [(Text, Value)] -> IO [Either ZapError Value]
batchCall (ZapClient stateMVar) calls = withMVar stateMVar $ \state -> do
    -- Generate request IDs
    requests <- mapM (\(name, args) -> do
        nextId <- atomicModifyIORef' (stNextId state) $ \n -> (n + 1, n)
        let reqId = "req-" <> T.pack (show nextId)
        pure Request
            { reqId        = reqId
            , reqToolName  = name
            , reqArguments = args
            , reqMetadata  = Nothing
            }
        ) calls

    -- Send batch request
    let batchReq = object ["requests" .= requests]
    let payload = BL.toStrict $ encode batchReq
    sendMessage (stSocket state) 0x14 payload

    -- Receive batch response
    (msgType, respPayload) <- recvMessage (stSocket state)
    case msgType of
        0x15 -> case decode' (BL.fromStrict respPayload) of
            Just (Object obj) -> case Map.lookup "responses" (toMapText obj) of
                Just (Array resps) -> pure $ map parseResp (map parseResponse (V.toList resps))
                _ -> pure $ replicate (length calls) $ Left ZapError
                    { errCode    = ErrParseError
                    , errMessage = "Invalid batch response format"
                    , errData    = Nothing
                    }
            _ -> pure $ replicate (length calls) $ Left ZapError
                { errCode    = ErrParseError
                , errMessage = "Failed to parse batch response"
                , errData    = Nothing
                }
        0xFE -> case decode' (BL.fromStrict respPayload) of
            Just err -> pure $ replicate (length calls) $ Left err
            Nothing  -> pure $ replicate (length calls) $ Left ZapError
                { errCode    = ErrInternalError
                , errMessage = "Failed to parse error"
                , errData    = Nothing
                }
        _ -> pure $ replicate (length calls) $ Left ZapError
            { errCode    = ErrInternalError
            , errMessage = "Unexpected message type"
            , errData    = Nothing
            }
  where
    parseResponse :: Value -> Maybe Response
    parseResponse v = decode' (encode v)

    parseResp :: Maybe Response -> Either ZapError Value
    parseResp Nothing = Left ZapError
        { errCode    = ErrParseError
        , errMessage = "Failed to parse response"
        , errData    = Nothing
        }
    parseResp (Just resp) = case respError resp of
        Just err -> Left err
        Nothing  -> Right $ maybe Null id (respContent resp)

-------------------------------------------------------------------------------
-- Low-level Operations
-------------------------------------------------------------------------------

-- | Send a request to the server.
sendRequest :: ZapClient -> Request -> IO ()
sendRequest (ZapClient stateMVar) req = withMVar stateMVar $ \state -> do
    let payload = BL.toStrict $ encode req
    sendMessage (stSocket state) 0x12 payload

-- | Receive a response for a specific request ID.
recvResponse :: ZapClient -> RequestId -> IO Response
recvResponse (ZapClient stateMVar) reqId = withMVar stateMVar $ \state -> do
    (msgType, payload) <- recvMessage (stSocket state)
    case msgType of
        0x13 -> case decode' (BL.fromStrict payload) of
            Just resp | respId resp == reqId -> pure resp
            Just _    -> fail "Response ID mismatch"
            Nothing   -> fail "Failed to parse response"
        0xFE -> case decode' (BL.fromStrict payload) of
            Just err -> pure Response
                { respId       = reqId
                , respContent  = Nothing
                , respError    = Just err
                , respMetadata = Nothing
                }
            Nothing -> fail "Failed to parse error response"
        _ -> fail $ "Unexpected message type: " <> show msgType

-- | Send a ping to keep the connection alive.
ping :: ZapClient -> IO Bool
ping (ZapClient stateMVar) = withMVar stateMVar $ \state -> do
    result <- try $ do
        sendMessage (stSocket state) 0xE0 BS.empty
        (msgType, _) <- recvMessage (stSocket state)
        pure (msgType == 0xE1)
    case result of
        Left (_ :: IOException) -> pure False
        Right b -> pure b

-------------------------------------------------------------------------------
-- State Queries
-------------------------------------------------------------------------------

-- | Check if the client is connected.
isConnected :: ZapClient -> IO Bool
isConnected (ZapClient stateMVar) = withMVar stateMVar $ \state ->
    readTVarIO (stConnected state)

-- | Get server capabilities.
getCapabilities :: ZapClient -> IO Capabilities
getCapabilities (ZapClient stateMVar) = withMVar stateMVar $ \state ->
    pure (stCapabilities state)

-- | Get the session ID.
getSessionId :: ZapClient -> IO Text
getSessionId (ZapClient stateMVar) = withMVar stateMVar $ \state ->
    pure (stSessionId state)

-------------------------------------------------------------------------------
-- Wire Protocol
-------------------------------------------------------------------------------

-- | Send a message with the ZAP wire format.
--
-- Wire format:
--
-- @
-- +----------+----------+----------------------+
-- |  Length  | MsgType  |      Payload         |
-- | (4 bytes)| (1 byte) |     (variable)       |
-- |  LE u32  |          |                      |
-- +----------+----------+----------------------+
-- @
sendMessage :: Socket -> Word8 -> BS.ByteString -> IO ()
sendMessage sock msgType payload = do
    let len = fromIntegral (BS.length payload + 1) :: Word32
    let header = BS.pack
            [ fromIntegral (len .&. 0xFF)
            , fromIntegral ((len `shiftR` 8) .&. 0xFF)
            , fromIntegral ((len `shiftR` 16) .&. 0xFF)
            , fromIntegral ((len `shiftR` 24) .&. 0xFF)
            , msgType
            ]
    sendAll sock (header <> payload)

-- | Receive a message from the socket.
recvMessage :: Socket -> IO (Word8, BS.ByteString)
recvMessage sock = do
    header <- recvExact sock 5
    let len = fromIntegral (BS.index header 0)
            + fromIntegral (BS.index header 1) * 256
            + fromIntegral (BS.index header 2) * 65536
            + fromIntegral (BS.index header 3) * 16777216 :: Int
    let msgType = BS.index header 4
    payload <- if len > 1
               then recvExact sock (len - 1)
               else pure BS.empty
    pure (msgType, payload)

-- | Receive exactly n bytes from a socket.
recvExact :: Socket -> Int -> IO BS.ByteString
recvExact sock n = go n BS.empty
  where
    go 0 acc = pure acc
    go remaining acc = do
        chunk <- recv sock (min remaining 4096)
        when (BS.null chunk) $
            fail "Connection closed"
        go (remaining - BS.length chunk) (acc <> chunk)
