{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Hanzo.Zap.Server
-- Copyright   : (c) 2025 Hanzo AI Inc.
-- License     : MIT
-- Maintainer  : dev@hanzo.ai
-- Stability   : experimental
-- Portability : portable
--
-- ZAP server implementation for exposing tools to AI agents.
--
-- = Quick Start
--
-- @
-- import Hanzo.Zap.Server
-- import Hanzo.Zap.Types
-- import Data.Aeson (Value(..), object, (.=))
--
-- main :: IO ()
-- main = do
--     let cfg = defaultServerConfig { srvPort = 9999 }
--     server <- newServer cfg
--     registerTool server readFileTool readFileHandler
--     runServer server
--
-- readFileTool :: Tool
-- readFileTool = Tool
--     { toolName = "read_file"
--     , toolDescription = "Read file contents"
--     , toolCategory = FileOps
--     , toolPermission = PermRead
--     , toolInputSchema = ...
--     }
--
-- readFileHandler :: Value -> IO ToolResult
-- readFileHandler args = ...
-- @

module Hanzo.Zap.Server
    ( -- * Server Type
      ZapServer
    , ServerConfig (..)
    , defaultServerConfig

      -- * Lifecycle
    , newServer
    , runServer
    , stopServer

      -- * Tool Registration
    , ToolHandler
    , registerTool
    , registerToolIO
    , unregisterTool

      -- * Configuration
    , setApprovalPolicy
    , setSandboxPolicy
    , getRegisteredTools

      -- * Middleware
    , Middleware
    , addMiddleware
    , loggingMiddleware
    , rateLimitMiddleware

      -- * Events
    , ServerEvent (..)
    , subscribeEvents
    ) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, withMVar, forkIO, killThread, ThreadId)
import Control.Concurrent.Async (async, wait, race)
import Control.Concurrent.STM
    ( TVar
    , TChan
    , newTVarIO
    , readTVarIO
    , atomically
    , writeTVar
    , modifyTVar'
    , newBroadcastTChanIO
    , dupTChan
    , readTChan
    , writeTChan
    )
import Control.Exception (bracket, finally, catch, IOException)
import Control.Monad (forever, void, when, forM_)
import Data.Aeson (Value (..), encode, decode', object, (.=))
import Data.Aeson.KeyMap (toMapText)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word8, Word32)
import GHC.Generics (Generic)
import Network.Socket
    ( Socket
    , AddrInfo (..)
    , SocketType (..)
    , SocketOption (..)
    , Family (..)
    , SockAddr (..)
    , getAddrInfo
    , socket
    , bind
    , listen
    , accept
    , close
    , setSocketOption
    , defaultHints
    , withSocketsDo
    )
import Network.Socket.ByteString (recv, sendAll)

import Hanzo.Zap.Types

-------------------------------------------------------------------------------
-- Server Configuration
-------------------------------------------------------------------------------

-- | Server configuration options.
data ServerConfig = ServerConfig
    { srvName           :: Text
    , srvVersion        :: Text
    , srvHost           :: String
    , srvPort           :: Int
    , srvMaxConnections :: Int
    , srvApprovalPolicy :: ApprovalPolicy
    , srvSandboxPolicy  :: SandboxPolicy
    }
    deriving stock (Eq, Show, Generic)

-- | Default server configuration.
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { srvName           = "hanzo-zap-haskell"
    , srvVersion        = "0.6.0"
    , srvHost           = "0.0.0.0"
    , srvPort           = 9999
    , srvMaxConnections = 100
    , srvApprovalPolicy = ApprovalOnRequest
    , srvSandboxPolicy  = SandboxWorkspaceWrite defaultSandboxConfig
    }

-------------------------------------------------------------------------------
-- Tool Handler
-------------------------------------------------------------------------------

-- | Handler function for tool execution.
type ToolHandler = Value -> IO ToolResult

-- | Registered tool with handler.
data RegisteredTool = RegisteredTool
    { rtTool    :: Tool
    , rtHandler :: ToolHandler
    }

-------------------------------------------------------------------------------
-- Server State
-------------------------------------------------------------------------------

-- | Internal server state.
data ServerState = ServerState
    { ssConfig      :: ServerConfig
    , ssSocket      :: Maybe Socket
    , ssTools       :: Map Text RegisteredTool
    , ssMiddleware  :: [Middleware]
    , ssConnections :: TVar Int
    , ssRunning     :: TVar Bool
    , ssEventChan   :: TChan ServerEvent
    , ssThreads     :: TVar [ThreadId]
    }

-- | ZAP server handle.
newtype ZapServer = ZapServer (MVar ServerState)

-------------------------------------------------------------------------------
-- Server Events
-------------------------------------------------------------------------------

-- | Events emitted by the server.
data ServerEvent
    = EventClientConnected SockAddr
    | EventClientDisconnected SockAddr
    | EventToolCalled Text RequestId
    | EventToolCompleted Text RequestId Double  -- ^ Tool, RequestId, DurationMs
    | EventToolError Text RequestId Text        -- ^ Tool, RequestId, Error
    | EventServerStarted Int                    -- ^ Port
    | EventServerStopped
    deriving stock (Eq, Show, Generic)

-------------------------------------------------------------------------------
-- Middleware
-------------------------------------------------------------------------------

-- | Middleware wraps tool execution.
type Middleware = Text -> Value -> ToolHandler -> IO ToolResult

-- | Logging middleware that logs tool calls.
loggingMiddleware :: (Text -> IO ()) -> Middleware
loggingMiddleware logFn toolName args next = do
    logFn $ "Calling tool: " <> toolName
    result <- next args
    logFn $ "Tool completed: " <> toolName
    pure result

-- | Rate limiting middleware.
rateLimitMiddleware :: Int -> IORef (Map Text UTCTime) -> Middleware
rateLimitMiddleware minIntervalMs stateRef toolName args next = do
    now <- getCurrentTime
    allowed <- atomicModifyIORef' stateRef $ \m ->
        case Map.lookup toolName m of
            Nothing -> (Map.insert toolName now m, True)
            Just lastCall ->
                let elapsed = diffUTCTime now lastCall * 1000
                in if elapsed >= fromIntegral minIntervalMs
                   then (Map.insert toolName now m, True)
                   else (m, False)

    if allowed
       then next args
       else pure ToolResult
           { resultContent  = [ContentItem "text" $ String "Rate limited"]
           , resultIsError  = True
           , resultMetadata = Nothing
           }

-------------------------------------------------------------------------------
-- Server Lifecycle
-------------------------------------------------------------------------------

-- | Create a new ZAP server.
newServer :: ServerConfig -> IO ZapServer
newServer cfg = do
    connections <- newTVarIO 0
    running <- newTVarIO False
    eventChan <- newBroadcastTChanIO
    threads <- newTVarIO []

    let state = ServerState
            { ssConfig      = cfg
            , ssSocket      = Nothing
            , ssTools       = Map.empty
            , ssMiddleware  = []
            , ssConnections = connections
            , ssRunning     = running
            , ssEventChan   = eventChan
            , ssThreads     = threads
            }

    ZapServer <$> newMVar state

-- | Run the server (blocking).
runServer :: ZapServer -> IO ()
runServer (ZapServer stateMVar) = withSocketsDo $ do
    state <- readMVar stateMVar

    -- Create socket
    let cfg = ssConfig state
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just $ srvHost cfg) (Just $ show $ srvPort cfg)

    case addrs of
        [] -> fail $ "Cannot resolve: " <> srvHost cfg
        (addr:_) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)
            listen sock (srvMaxConnections cfg)

            -- Update state
            modifyMVar_ stateMVar $ \s -> pure s { ssSocket = Just sock }
            atomically $ writeTVar (ssRunning state) True

            -- Emit start event
            atomically $ writeTChan (ssEventChan state) $ EventServerStarted (srvPort cfg)

            -- Accept loop
            acceptLoop (ZapServer stateMVar) sock `finally` do
                atomically $ writeTVar (ssRunning state) False
                atomically $ writeTChan (ssEventChan state) EventServerStopped
                close sock
  where
    readMVar = withMVar stateMVar pure

-- | Accept incoming connections.
acceptLoop :: ZapServer -> Socket -> IO ()
acceptLoop server@(ZapServer stateMVar) sock = do
    running <- withMVar stateMVar $ \s -> readTVarIO (ssRunning s)
    when running $ do
        result <- try $ accept sock
        case result of
            Left (_ :: IOException) -> pure ()
            Right (clientSock, clientAddr) -> do
                -- Spawn client handler
                state <- withMVar stateMVar pure
                atomically $ modifyTVar' (ssConnections state) (+ 1)
                atomically $ writeTChan (ssEventChan state) $ EventClientConnected clientAddr

                tid <- forkIO $ handleClient server clientSock clientAddr `finally` do
                    atomically $ modifyTVar' (ssConnections state) (subtract 1)
                    atomically $ writeTChan (ssEventChan state) $ EventClientDisconnected clientAddr
                    close clientSock

                atomically $ modifyTVar' (ssThreads state) (tid :)
                acceptLoop server sock
  where
    try :: IO a -> IO (Either IOException a)
    try = Control.Exception.catch (fmap Right) (pure . Left)

-- | Stop the server.
stopServer :: ZapServer -> IO ()
stopServer (ZapServer stateMVar) = modifyMVar_ stateMVar $ \state -> do
    atomically $ writeTVar (ssRunning state) False

    -- Close socket
    case ssSocket state of
        Nothing   -> pure ()
        Just sock -> close sock

    -- Kill client threads
    threads <- readTVarIO (ssThreads state)
    forM_ threads killThread

    pure state { ssSocket = Nothing }

-------------------------------------------------------------------------------
-- Client Handling
-------------------------------------------------------------------------------

-- | Handle a connected client.
handleClient :: ZapServer -> Socket -> SockAddr -> IO ()
handleClient server@(ZapServer stateMVar) sock _addr = do
    -- Wait for Init message
    (msgType, payload) <- recvMessage sock
    when (msgType /= 0x01) $
        fail $ "Expected Init (0x01), got: " <> show msgType

    case decode' (BL.fromStrict payload) of
        Nothing -> fail "Failed to parse Init message"
        Just (initMsg :: InitMessage) -> do
            -- Send InitAck
            state <- withMVar stateMVar pure
            let ack = InitAck
                    { ackName         = srvName (ssConfig state)
                    , ackVersion      = srvVersion (ssConfig state)
                    , ackCapabilities = Capabilities True False False True
                    }
            let ackPayload = BL.toStrict $ encode ack
            sendMessage sock 0x02 ackPayload

            -- Message loop
            messageLoop server sock

-- | Main message processing loop.
messageLoop :: ZapServer -> Socket -> IO ()
messageLoop server@(ZapServer stateMVar) sock = do
    result <- try $ recvMessage sock
    case result of
        Left (_ :: IOException) -> pure ()  -- Connection closed
        Right (msgType, payload) -> do
            handleMessage server sock msgType payload
            messageLoop server sock
  where
    try :: IO a -> IO (Either IOException a)
    try = Control.Exception.catch (fmap Right) (pure . Left)

-- | Handle a single message.
handleMessage :: ZapServer -> Socket -> Word8 -> BS.ByteString -> IO ()
handleMessage (ZapServer stateMVar) sock msgType payload = case msgType of
    -- ListTools
    0x10 -> do
        state <- withMVar stateMVar pure
        let tools = map rtTool $ Map.elems (ssTools state)
        let resp = BL.toStrict $ encode tools
        sendMessage sock 0x11 resp

    -- CallTool
    0x12 -> case decode' (BL.fromStrict payload) of
        Nothing -> sendError sock ErrParseError "Failed to parse request"
        Just req -> do
            state <- withMVar stateMVar pure
            atomically $ writeTChan (ssEventChan state) $
                EventToolCalled (reqToolName req) (reqId req)

            startTime <- getCurrentTime

            result <- case Map.lookup (reqToolName req) (ssTools state) of
                Nothing -> pure $ Left ZapError
                    { errCode    = ErrMethodNotFound
                    , errMessage = "Tool not found: " <> reqToolName req
                    , errData    = Nothing
                    }
                Just rt -> do
                    -- Apply middleware chain
                    let handler = rtHandler rt
                    let chain = foldr applyMiddleware handler (ssMiddleware state)
                    Right <$> chain (reqToolName req) (reqArguments req) handler

            endTime <- getCurrentTime
            let durationMs = realToFrac (diffUTCTime endTime startTime) * 1000

            case result of
                Left err -> do
                    atomically $ writeTChan (ssEventChan state) $
                        EventToolError (reqToolName req) (reqId req) (errMessage err)
                    let resp = Response
                            { respId       = reqId req
                            , respContent  = Nothing
                            , respError    = Just err
                            , respMetadata = Nothing
                            }
                    sendMessage sock 0x13 (BL.toStrict $ encode resp)

                Right toolResult -> do
                    atomically $ writeTChan (ssEventChan state) $
                        EventToolCompleted (reqToolName req) (reqId req) durationMs

                    let content = if null (resultContent toolResult)
                                  then Nothing
                                  else Just $ toJSON (resultContent toolResult)
                    let resp = Response
                            { respId       = reqId req
                            , respContent  = content
                            , respError    = if resultIsError toolResult
                                             then Just ZapError
                                                 { errCode    = ErrInternalError
                                                 , errMessage = "Tool execution failed"
                                                 , errData    = content
                                                 }
                                             else Nothing
                            , respMetadata = resultMetadata toolResult
                            }
                    sendMessage sock 0x13 (BL.toStrict $ encode resp)

    -- Ping
    0xE0 -> sendMessage sock 0xE1 BS.empty

    -- Cancel (acknowledge but don't actually cancel for now)
    0xF0 -> case decode' (BL.fromStrict payload) of
        Nothing -> pure ()
        Just obj -> case Map.lookup "request_id" (obj :: Map Text Value) of
            Just (String reqId) -> do
                let ack = object ["request_id" .= reqId, "cancelled" .= True]
                sendMessage sock 0xF1 (BL.toStrict $ encode ack)
            _ -> pure ()

    -- Unknown
    _ -> sendError sock ErrInvalidRequest $ "Unknown message type: " <> T.pack (show msgType)

  where
    applyMiddleware :: Middleware -> ToolHandler -> Text -> Value -> ToolHandler -> IO ToolResult
    applyMiddleware mw next name args _ = mw name args next

    toJSON = encode

-- | Send an error response.
sendError :: Socket -> ErrorCode -> Text -> IO ()
sendError sock code msg = do
    let err = ZapError { errCode = code, errMessage = msg, errData = Nothing }
    sendMessage sock 0xFE (BL.toStrict $ encode err)

-------------------------------------------------------------------------------
-- Tool Registration
-------------------------------------------------------------------------------

-- | Register a tool with a pure handler.
registerTool :: ZapServer -> Tool -> ToolHandler -> IO ()
registerTool = registerToolIO

-- | Register a tool with an IO handler.
registerToolIO :: ZapServer -> Tool -> ToolHandler -> IO ()
registerToolIO (ZapServer stateMVar) tool handler =
    modifyMVar_ stateMVar $ \state -> pure state
        { ssTools = Map.insert (toolName tool) (RegisteredTool tool handler) (ssTools state)
        }

-- | Unregister a tool.
unregisterTool :: ZapServer -> Text -> IO ()
unregisterTool (ZapServer stateMVar) name =
    modifyMVar_ stateMVar $ \state -> pure state
        { ssTools = Map.delete name (ssTools state)
        }

-- | Get all registered tools.
getRegisteredTools :: ZapServer -> IO [Tool]
getRegisteredTools (ZapServer stateMVar) = withMVar stateMVar $ \state ->
    pure $ map rtTool $ Map.elems (ssTools state)

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Set the approval policy.
setApprovalPolicy :: ZapServer -> ApprovalPolicy -> IO ()
setApprovalPolicy (ZapServer stateMVar) policy =
    modifyMVar_ stateMVar $ \state -> pure state
        { ssConfig = (ssConfig state) { srvApprovalPolicy = policy }
        }

-- | Set the sandbox policy.
setSandboxPolicy :: ZapServer -> SandboxPolicy -> IO ()
setSandboxPolicy (ZapServer stateMVar) policy =
    modifyMVar_ stateMVar $ \state -> pure state
        { ssConfig = (ssConfig state) { srvSandboxPolicy = policy }
        }

-- | Add middleware to the server.
addMiddleware :: ZapServer -> Middleware -> IO ()
addMiddleware (ZapServer stateMVar) mw =
    modifyMVar_ stateMVar $ \state -> pure state
        { ssMiddleware = mw : ssMiddleware state
        }

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | Subscribe to server events.
subscribeEvents :: ZapServer -> IO (IO ServerEvent)
subscribeEvents (ZapServer stateMVar) = do
    state <- withMVar stateMVar pure
    chan <- atomically $ dupTChan (ssEventChan state)
    pure $ atomically $ readTChan chan

-------------------------------------------------------------------------------
-- Wire Protocol
-------------------------------------------------------------------------------

-- | Send a message with the ZAP wire format.
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
