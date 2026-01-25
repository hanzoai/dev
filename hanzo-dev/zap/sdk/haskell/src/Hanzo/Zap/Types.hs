{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Hanzo.Zap.Types
-- Copyright   : (c) 2025 Hanzo AI Inc.
-- License     : MIT
-- Maintainer  : dev@hanzo.ai
-- Stability   : experimental
-- Portability : portable
--
-- Core types for the ZAP (Zero-copy Agent Protocol).
--
-- This module provides typed representations for:
--
-- * Tool categories ('ToolCategory') with operations like 'FileOps', 'GitOps', etc.
-- * Message types ('Request', 'Response', 'Notification')
-- * Security policies ('ApprovalPolicy', 'SandboxPolicy')
-- * Error handling ('ZapError', 'ErrorCode')
--
-- All types derive 'Generic' and have 'Aeson' instances for JSON serialization.

module Hanzo.Zap.Types
    ( -- * Tool Categories
      ToolCategory (..)
    , Tool (..)
    , ToolSchema (..)
    , Permission (..)

      -- * Message Types
    , Request (..)
    , Response (..)
    , Notification (..)
    , MessageType (..)
    , RequestId

      -- * Tool Results
    , ToolResult (..)
    , ContentItem (..)

      -- * Security Policies
    , ApprovalPolicy (..)
    , SandboxPolicy (..)
    , SandboxConfig (..)
    , defaultSandboxConfig

      -- * Errors
    , ZapError (..)
    , ErrorCode (..)
    , errorCodeToInt
    , intToErrorCode

      -- * Connection
    , Capabilities (..)
    , InitMessage (..)
    , InitAck (..)

      -- * Metadata
    , Metadata
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Request ID
-------------------------------------------------------------------------------

-- | Unique identifier for requests, used to match responses.
type RequestId = Text

-- | Arbitrary metadata attached to requests/responses.
type Metadata = Map Text Value

-------------------------------------------------------------------------------
-- Tool Categories
-------------------------------------------------------------------------------

-- | Categories of tools available in ZAP.
--
-- ZAP defines 14 tool categories with 167+ typed operations.
data ToolCategory
    = -- | Filesystem operations: read_file, write_file, edit_file, glob, grep
      FileOps
    | -- | Version control: git_status, git_diff, git_commit, git_log
      GitOps
    | -- | Process execution: exec, list_processes, kill_process
      ExecOps
    | -- | Build system: build, test, lint, typecheck
      BuildOps
    | -- | Network operations: http_request, fetch_url, port_check
      NetworkOps
    | -- | Browser automation: navigate, click, fill, screenshot
      BrowserOps
    | -- | Language server: completion, definition, references, rename
      LspOps
    | -- | Debugging: breakpoint, step, inspect, profile
      DebugOps
    | -- | Containers: docker_run, k8s_apply, vm_create
      ContainerOps
    | -- | Cloud/IaC: deploy, secrets, dns
      CloudOps
    | -- | Database: query, migrate, backup
      DataOps
    | -- | Security: scan, sign, verify
      SecurityOps
    | -- | Computer vision: ocr, detect_ui, describe_screen
      VisionOps
    | -- | Planning: plan_intent, plan_route, audit_log
      PlanOps
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON ToolCategory where
    toJSON = \case
        FileOps      -> "file"
        GitOps       -> "git"
        ExecOps      -> "exec"
        BuildOps     -> "build"
        NetworkOps   -> "network"
        BrowserOps   -> "browser"
        LspOps       -> "lsp"
        DebugOps     -> "debug"
        ContainerOps -> "container"
        CloudOps     -> "cloud"
        DataOps      -> "data"
        SecurityOps  -> "security"
        VisionOps    -> "vision"
        PlanOps      -> "plan"

instance FromJSON ToolCategory where
    parseJSON = withText "ToolCategory" $ \case
        "file"      -> pure FileOps
        "git"       -> pure GitOps
        "exec"      -> pure ExecOps
        "build"     -> pure BuildOps
        "network"   -> pure NetworkOps
        "browser"   -> pure BrowserOps
        "lsp"       -> pure LspOps
        "debug"     -> pure DebugOps
        "container" -> pure ContainerOps
        "cloud"     -> pure CloudOps
        "data"      -> pure DataOps
        "security"  -> pure SecurityOps
        "vision"    -> pure VisionOps
        "plan"      -> pure PlanOps
        other       -> fail $ "Unknown tool category: " <> show other

-------------------------------------------------------------------------------
-- Tool Schema
-------------------------------------------------------------------------------

-- | Permission level required to execute a tool.
data Permission
    = PermRead    -- ^ Read-only operations (file read, git status)
    | PermWrite   -- ^ Write operations (file write, git commit)
    | PermExecute -- ^ Execution (shell commands, builds)
    | PermAdmin   -- ^ Administrative (kill process, deploy)
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON Permission where
    toJSON = \case
        PermRead    -> "read"
        PermWrite   -> "write"
        PermExecute -> "execute"
        PermAdmin   -> "admin"

instance FromJSON Permission where
    parseJSON = withText "Permission" $ \case
        "read"    -> pure PermRead
        "write"   -> pure PermWrite
        "execute" -> pure PermExecute
        "admin"   -> pure PermAdmin
        other     -> fail $ "Unknown permission: " <> show other

-- | JSON Schema for tool input parameters.
data ToolSchema = ToolSchema
    { schemaType       :: Text
    , schemaRequired   :: [Text]
    , schemaProperties :: Map Text Value
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON ToolSchema where
    toJSON ToolSchema{..} = object
        [ "type"       .= schemaType
        , "required"   .= schemaRequired
        , "properties" .= schemaProperties
        ]

instance FromJSON ToolSchema where
    parseJSON = withObject "ToolSchema" $ \o -> do
        schemaType       <- o .: "type"
        schemaRequired   <- o .:? "required"   >>= pure . maybe [] id
        schemaProperties <- o .:? "properties" >>= pure . maybe Map.empty id
        pure ToolSchema{..}

-- | A tool definition with name, description, and schema.
data Tool = Tool
    { toolName        :: Text
    , toolDescription :: Text
    , toolCategory    :: ToolCategory
    , toolPermission  :: Permission
    , toolInputSchema :: ToolSchema
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Tool where
    toJSON Tool{..} = object
        [ "name"        .= toolName
        , "description" .= toolDescription
        , "category"    .= toolCategory
        , "permission"  .= toolPermission
        , "inputSchema" .= toolInputSchema
        ]

instance FromJSON Tool where
    parseJSON = withObject "Tool" $ \o -> do
        toolName        <- o .: "name"
        toolDescription <- o .: "description"
        toolCategory    <- o .:? "category"   >>= pure . maybe FileOps id
        toolPermission  <- o .:? "permission" >>= pure . maybe PermRead id
        toolInputSchema <- o .: "inputSchema"
        pure Tool{..}

-------------------------------------------------------------------------------
-- Message Types
-------------------------------------------------------------------------------

-- | ZAP message type identifiers.
data MessageType
    = MsgInit           -- ^ 0x01: Client initialization
    | MsgInitAck        -- ^ 0x02: Server acknowledgment
    | MsgListTools      -- ^ 0x10: List available tools
    | MsgToolList       -- ^ 0x11: Tool list response
    | MsgCallTool       -- ^ 0x12: Call a tool
    | MsgToolResult     -- ^ 0x13: Tool result
    | MsgBatchCall      -- ^ 0x14: Batch tool calls
    | MsgBatchResult    -- ^ 0x15: Batch results
    | MsgNotification   -- ^ 0x20: One-way notification
    | MsgPing           -- ^ 0xE0: Keep-alive ping
    | MsgPong           -- ^ 0xE1: Keep-alive pong
    | MsgCancel         -- ^ 0xF0: Cancel request
    | MsgCancelAck      -- ^ 0xF1: Cancel acknowledgment
    | MsgError          -- ^ 0xFE: Error response
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON MessageType where
    toJSON = \case
        MsgInit         -> Number 0x01
        MsgInitAck      -> Number 0x02
        MsgListTools    -> Number 0x10
        MsgToolList     -> Number 0x11
        MsgCallTool     -> Number 0x12
        MsgToolResult   -> Number 0x13
        MsgBatchCall    -> Number 0x14
        MsgBatchResult  -> Number 0x15
        MsgNotification -> Number 0x20
        MsgPing         -> Number 0xE0
        MsgPong         -> Number 0xE1
        MsgCancel       -> Number 0xF0
        MsgCancelAck    -> Number 0xF1
        MsgError        -> Number 0xFE

-- | A request to call a tool.
data Request = Request
    { reqId        :: RequestId
    , reqToolName  :: Text
    , reqArguments :: Value
    , reqMetadata  :: Maybe Metadata
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Request where
    toJSON Request{..} = object $
        [ "id"        .= reqId
        , "name"      .= reqToolName
        , "arguments" .= reqArguments
        ] <> maybe [] (\m -> ["metadata" .= m]) reqMetadata

instance FromJSON Request where
    parseJSON = withObject "Request" $ \o -> do
        reqId        <- o .: "id"
        reqToolName  <- o .: "name"
        reqArguments <- o .: "arguments"
        reqMetadata  <- o .:? "metadata"
        pure Request{..}

-- | A response to a tool call.
data Response = Response
    { respId       :: RequestId
    , respContent  :: Maybe Value
    , respError    :: Maybe ZapError
    , respMetadata :: Maybe Metadata
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Response where
    toJSON Response{..} = object $
        [ "id" .= respId ]
        <> maybe [] (\c -> ["content"  .= c]) respContent
        <> maybe [] (\e -> ["error"    .= e]) respError
        <> maybe [] (\m -> ["metadata" .= m]) respMetadata

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> do
        respId       <- o .: "id"
        respContent  <- o .:? "content"
        respError    <- o .:? "error"
        respMetadata <- o .:? "metadata"
        pure Response{..}

-- | A one-way notification (no response expected).
data Notification = Notification
    { notifyMethod :: Text
    , notifyParams :: Maybe Value
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Notification where
    toJSON Notification{..} = object $
        [ "method" .= notifyMethod ]
        <> maybe [] (\p -> ["params" .= p]) notifyParams

instance FromJSON Notification where
    parseJSON = withObject "Notification" $ \o -> do
        notifyMethod <- o .: "method"
        notifyParams <- o .:? "params"
        pure Notification{..}

-------------------------------------------------------------------------------
-- Tool Results
-------------------------------------------------------------------------------

-- | Content item in a tool result.
data ContentItem = ContentItem
    { contentType :: Text      -- ^ MIME type
    , contentData :: Value     -- ^ Content data (text, base64, etc.)
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON ContentItem where
    toJSON ContentItem{..} = object
        [ "type" .= contentType
        , "data" .= contentData
        ]

instance FromJSON ContentItem where
    parseJSON = withObject "ContentItem" $ \o -> do
        contentType <- o .: "type"
        contentData <- o .: "data"
        pure ContentItem{..}

-- | Result of a tool execution.
data ToolResult = ToolResult
    { resultContent  :: [ContentItem]
    , resultIsError  :: Bool
    , resultMetadata :: Maybe Metadata
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON ToolResult where
    toJSON ToolResult{..} = object $
        [ "content"  .= resultContent
        , "isError"  .= resultIsError
        ] <> maybe [] (\m -> ["metadata" .= m]) resultMetadata

instance FromJSON ToolResult where
    parseJSON = withObject "ToolResult" $ \o -> do
        resultContent  <- o .: "content"
        resultIsError  <- o .:? "isError" >>= pure . maybe False id
        resultMetadata <- o .:? "metadata"
        pure ToolResult{..}

-------------------------------------------------------------------------------
-- Security Policies
-------------------------------------------------------------------------------

-- | Approval policy for tool execution.
--
-- Determines when the agent should pause and ask for human approval.
data ApprovalPolicy
    = -- | Full autonomy - never ask (suitable for CI/CD pipelines)
      ApprovalNever
    | -- | Auto-approve, but ask when operations fail
      ApprovalOnFailure
    | -- | Model decides based on risk assessment (default)
      ApprovalOnRequest
    | -- | Ask for everything except known-safe read operations
      ApprovalUnlessTrusted
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON ApprovalPolicy where
    toJSON = \case
        ApprovalNever         -> "never"
        ApprovalOnFailure     -> "on-failure"
        ApprovalOnRequest     -> "on-request"
        ApprovalUnlessTrusted -> "unless-trusted"

instance FromJSON ApprovalPolicy where
    parseJSON = withText "ApprovalPolicy" $ \case
        "never"          -> pure ApprovalNever
        "on-failure"     -> pure ApprovalOnFailure
        "on-request"     -> pure ApprovalOnRequest
        "unless-trusted" -> pure ApprovalUnlessTrusted
        other            -> fail $ "Unknown approval policy: " <> show other

-- | Configuration for workspace-write sandbox.
data SandboxConfig = SandboxConfig
    { sandboxWritableRoots   :: [FilePath]
    , sandboxNetworkAccess   :: Bool
    , sandboxExcludeTmpdir   :: Bool
    , sandboxExcludeSlashTmp :: Bool
    , sandboxAllowGitWrites  :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON SandboxConfig where
    toJSON SandboxConfig{..} = object
        [ "writable_roots"        .= sandboxWritableRoots
        , "network_access"        .= sandboxNetworkAccess
        , "exclude_tmpdir_env"    .= sandboxExcludeTmpdir
        , "exclude_slash_tmp"     .= sandboxExcludeSlashTmp
        , "allow_git_writes"      .= sandboxAllowGitWrites
        ]

instance FromJSON SandboxConfig where
    parseJSON = withObject "SandboxConfig" $ \o -> do
        sandboxWritableRoots   <- o .:? "writable_roots"     >>= pure . maybe [] id
        sandboxNetworkAccess   <- o .:? "network_access"     >>= pure . maybe True id
        sandboxExcludeTmpdir   <- o .:? "exclude_tmpdir_env" >>= pure . maybe False id
        sandboxExcludeSlashTmp <- o .:? "exclude_slash_tmp"  >>= pure . maybe False id
        sandboxAllowGitWrites  <- o .:? "allow_git_writes"   >>= pure . maybe False id
        pure SandboxConfig{..}

-- | Default sandbox configuration.
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig
    { sandboxWritableRoots   = []
    , sandboxNetworkAccess   = True
    , sandboxExcludeTmpdir   = False
    , sandboxExcludeSlashTmp = False
    , sandboxAllowGitWrites  = False
    }

-- | Sandbox policy for execution restrictions.
--
-- Determines what operations are physically allowed.
data SandboxPolicy
    = -- | Full access - no restrictions (use with caution)
      SandboxDangerFullAccess
    | -- | Read-only filesystem access, no network
      SandboxReadOnly
    | -- | Write only to workspace and specified roots
      SandboxWorkspaceWrite SandboxConfig
    deriving stock (Eq, Show, Generic)

instance ToJSON SandboxPolicy where
    toJSON = \case
        SandboxDangerFullAccess  -> "danger-full-access"
        SandboxReadOnly          -> "read-only"
        SandboxWorkspaceWrite cfg -> object
            [ "mode"   .= ("workspace-write" :: Text)
            , "config" .= cfg
            ]

instance FromJSON SandboxPolicy where
    parseJSON (String "danger-full-access") = pure SandboxDangerFullAccess
    parseJSON (String "read-only")          = pure SandboxReadOnly
    parseJSON v = withObject "SandboxPolicy" (\o -> do
        mode <- o .: "mode"
        case (mode :: Text) of
            "workspace-write" -> SandboxWorkspaceWrite <$> o .: "config"
            _                 -> fail $ "Unknown sandbox mode: " <> show mode
        ) v

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

-- | Standard ZAP error codes (JSON-RPC compatible).
data ErrorCode
    = ErrParseError      -- ^ -32700: Invalid JSON
    | ErrInvalidRequest  -- ^ -32600: Request structure invalid
    | ErrMethodNotFound  -- ^ -32601: Tool not found
    | ErrInvalidParams   -- ^ -32602: Arguments invalid
    | ErrInternalError   -- ^ -32603: Server-side error
    | ErrServerError     -- ^ -32000: Generic server error
    | ErrTimeout         -- ^ -32001: Request timed out
    | ErrPermissionDenied -- ^ -32002: Not authorized
    | ErrCancelled       -- ^ -32003: Request cancelled
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Convert error code to integer.
errorCodeToInt :: ErrorCode -> Int
errorCodeToInt = \case
    ErrParseError       -> -32700
    ErrInvalidRequest   -> -32600
    ErrMethodNotFound   -> -32601
    ErrInvalidParams    -> -32602
    ErrInternalError    -> -32603
    ErrServerError      -> -32000
    ErrTimeout          -> -32001
    ErrPermissionDenied -> -32002
    ErrCancelled        -> -32003

-- | Convert integer to error code.
intToErrorCode :: Int -> Maybe ErrorCode
intToErrorCode = \case
    (-32700) -> Just ErrParseError
    (-32600) -> Just ErrInvalidRequest
    (-32601) -> Just ErrMethodNotFound
    (-32602) -> Just ErrInvalidParams
    (-32603) -> Just ErrInternalError
    (-32000) -> Just ErrServerError
    (-32001) -> Just ErrTimeout
    (-32002) -> Just ErrPermissionDenied
    (-32003) -> Just ErrCancelled
    _        -> Nothing

instance ToJSON ErrorCode where
    toJSON = toJSON . errorCodeToInt

instance FromJSON ErrorCode where
    parseJSON v = do
        code <- parseJSON v
        case intToErrorCode code of
            Just ec -> pure ec
            Nothing -> fail $ "Unknown error code: " <> show code

-- | A ZAP error with code, message, and optional data.
data ZapError = ZapError
    { errCode    :: ErrorCode
    , errMessage :: Text
    , errData    :: Maybe Value
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON ZapError where
    toJSON ZapError{..} = object $
        [ "code"    .= errCode
        , "message" .= errMessage
        ] <> maybe [] (\d -> ["data" .= d]) errData

instance FromJSON ZapError where
    parseJSON = withObject "ZapError" $ \o -> do
        errCode    <- o .: "code"
        errMessage <- o .: "message"
        errData    <- o .:? "data"
        pure ZapError{..}

-------------------------------------------------------------------------------
-- Connection
-------------------------------------------------------------------------------

-- | Server capabilities advertised in handshake.
data Capabilities = Capabilities
    { capTools     :: Bool
    , capResources :: Bool
    , capPrompts   :: Bool
    , capLogging   :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Capabilities where
    toJSON Capabilities{..} = object
        [ "tools"     .= capTools
        , "resources" .= capResources
        , "prompts"   .= capPrompts
        , "logging"   .= capLogging
        ]

instance FromJSON Capabilities where
    parseJSON = withObject "Capabilities" $ \o -> do
        capTools     <- o .:? "tools"     >>= pure . maybe False id
        capResources <- o .:? "resources" >>= pure . maybe False id
        capPrompts   <- o .:? "prompts"   >>= pure . maybe False id
        capLogging   <- o .:? "logging"   >>= pure . maybe False id
        pure Capabilities{..}

-- | Client initialization message.
data InitMessage = InitMessage
    { initName    :: Text
    , initVersion :: Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON InitMessage where
    toJSON InitMessage{..} = object
        [ "name"    .= initName
        , "version" .= initVersion
        ]

instance FromJSON InitMessage where
    parseJSON = withObject "InitMessage" $ \o -> do
        initName    <- o .: "name"
        initVersion <- o .: "version"
        pure InitMessage{..}

-- | Server initialization acknowledgment.
data InitAck = InitAck
    { ackName         :: Text
    , ackVersion      :: Text
    , ackCapabilities :: Capabilities
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON InitAck where
    toJSON InitAck{..} = object
        [ "name"         .= ackName
        , "version"      .= ackVersion
        , "capabilities" .= ackCapabilities
        ]

instance FromJSON InitAck where
    parseJSON = withObject "InitAck" $ \o -> do
        ackName         <- o .: "name"
        ackVersion      <- o .: "version"
        ackCapabilities <- o .: "capabilities"
        pure InitAck{..}
