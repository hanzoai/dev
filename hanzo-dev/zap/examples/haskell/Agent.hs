{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | ZAP Agent Example - Haskell
--
-- Demonstrates a complete ZAP agent with tool execution in Haskell.
--
-- Usage:
--   cabal run zap-agent-haskell
--
-- Or with stack:
--   stack run

module Main where

import Control.Exception (IOException, catch)
import Control.Monad (forM)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesDirectoryExist, getFileSize, getCurrentDirectory, makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.Posix.Process (getProcessID)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)


-- | Approval policy enum (matches hanzo-protocol)
-- Determines when to ask for user approval before tool execution
data ApprovalPolicy
    = Untrusted    -- ^ Only auto-approve known-safe read operations
    | OnFailure    -- ^ Auto-approve, escalate on failure
    | OnRequest    -- ^ Model decides when to ask (default)
    | Never        -- ^ Never ask, return failures to model
    deriving (Show, Eq, Enum, Bounded)

instance ToJSON ApprovalPolicy where
    toJSON = \case
        Untrusted  -> "untrusted"
        OnFailure  -> "on-failure"
        OnRequest  -> "on-request"
        Never      -> "never"


-- | Sandbox policy enum (matches hanzo-protocol)
-- Determines execution restrictions for the agent
data SandboxPolicy
    = DangerFullAccess  -- ^ No restrictions (use with caution)
    | ReadOnly          -- ^ Read-only filesystem access
    | WorkspaceWrite    -- ^ Write only to cwd and specified roots
        { writableRoots       :: [FilePath]
        , networkAccess       :: Bool
        , excludeTmpdirEnvVar :: Bool
        , excludeSlashTmp     :: Bool
        , allowGitWrites      :: Bool
        }
    deriving (Show, Eq)

instance ToJSON SandboxPolicy where
    toJSON DangerFullAccess = "danger-full-access"
    toJSON ReadOnly = "read-only"
    toJSON (WorkspaceWrite roots net tmp slashTmp git) =
        object [ "mode" .= ("workspace-write" :: Text)
               , "writable_roots" .= roots
               , "network_access" .= net
               , "exclude_tmpdir_env_var" .= tmp
               , "exclude_slash_tmp" .= slashTmp
               , "allow_git_writes" .= git
               ]

-- | Default workspace-write policy
defaultWorkspaceWrite :: SandboxPolicy
defaultWorkspaceWrite = WorkspaceWrite
    { writableRoots       = []
    , networkAccess       = True
    , excludeTmpdirEnvVar = False
    , excludeSlashTmp     = False
    , allowGitWrites      = False
    }


-- | Execution context for ZAP agent
-- Contains all state needed for tool execution
data ExecutorContext = ExecutorContext
    { ctxCwd            :: FilePath
    , ctxEnv            :: Map String String
    , ctxSessionId      :: Maybe Text
    , ctxApprovalPolicy :: ApprovalPolicy
    , ctxSandboxPolicy  :: SandboxPolicy
    , ctxTimeoutMs      :: Maybe Int
    } deriving (Show, Eq)

instance ToJSON ExecutorContext where
    toJSON ExecutorContext{..} = object
        [ "cwd"             .= ctxCwd
        , "session_id"      .= ctxSessionId
        , "approval_policy" .= ctxApprovalPolicy
        , "sandbox_policy"  .= ctxSandboxPolicy
        , "timeout_ms"      .= ctxTimeoutMs
        ]

-- | Create a new ExecutorContext with sensible defaults
newExecutorContext :: FilePath -> IO ExecutorContext
newExecutorContext cwd = do
    absCwd <- makeAbsolute cwd
    env <- Map.fromList <$> getEnvironment
    pid <- getProcessID
    pure ExecutorContext
        { ctxCwd            = absCwd
        , ctxEnv            = env
        , ctxSessionId      = Just $ T.pack $ "haskell-agent-" <> show pid
        , ctxApprovalPolicy = OnRequest
        , ctxSandboxPolicy  = defaultWorkspaceWrite
        , ctxTimeoutMs      = Just 30000
        }


-- | Tool execution result
-- Uses Either for idiomatic Haskell error handling
data ToolResult a
    = ToolSuccess a
    | ToolError Text
    deriving (Show, Eq)

instance Functor ToolResult where
    fmap f (ToolSuccess a) = ToolSuccess (f a)
    fmap _ (ToolError e)   = ToolError e

instance Applicative ToolResult where
    pure = ToolSuccess
    ToolSuccess f <*> ToolSuccess a = ToolSuccess (f a)
    ToolError e   <*> _             = ToolError e
    _             <*> ToolError e   = ToolError e

instance Monad ToolResult where
    ToolSuccess a >>= f = f a
    ToolError e   >>= _ = ToolError e

-- | Convert IO exceptions to ToolResult
tryIO :: IO a -> IO (ToolResult a)
tryIO action = (ToolSuccess <$> action) `catch` handler
  where
    handler :: IOException -> IO (ToolResult a)
    handler e = pure $ ToolError $ T.pack $ show e


-- | Directory entry information
data DirEntry = DirEntry
    { entryName  :: Text
    , entryIsDir :: Bool
    , entrySize  :: Integer
    } deriving (Show, Eq)

instance ToJSON DirEntry where
    toJSON DirEntry{..} = object
        [ "name"   .= entryName
        , "is_dir" .= entryIsDir
        , "size"   .= entrySize
        ]


-- | ZAP Agent type
data ZapAgent = ZapAgent
    { agentCtx :: ExecutorContext
    } deriving (Show, Eq)

-- | Create a new ZAP agent
newZapAgent :: FilePath -> IO ZapAgent
newZapAgent cwd = ZapAgent <$> newExecutorContext cwd


-- | Available tools in this agent
listTools :: [Text]
listTools =
    [ "read_file"
    , "list_dir"
    , "git_status"
    , "git_log"
    , "exec"
    ]


-- | Read file contents
readFile' :: ExecutorContext -> FilePath -> IO (ToolResult Text)
readFile' ctx path = do
    let fullPath = ctxCwd ctx </> path
    tryIO $ TIO.readFile fullPath


-- | List directory contents
listDir :: ExecutorContext -> FilePath -> Bool -> IO (ToolResult [DirEntry])
listDir ctx path showHidden = do
    let fullPath = ctxCwd ctx </> path
    result <- tryIO $ listDirectory fullPath
    case result of
        ToolError e -> pure $ ToolError e
        ToolSuccess entries -> do
            let filtered = if showHidden
                           then entries
                           else filter (not . isHidden) entries
            ToolSuccess <$> forM filtered (mkEntry fullPath)
  where
    isHidden name = take 1 name == "."
    mkEntry dir name = do
        let entryPath = dir </> name
        isDir <- doesDirectoryExist entryPath
        size <- if isDir then pure 0 else getFileSize entryPath
        pure DirEntry
            { entryName  = T.pack name
            , entryIsDir = isDir
            , entrySize  = size
            }


-- | Get git status
gitStatus :: ExecutorContext -> IO (ToolResult Text)
gitStatus ctx = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode
        "git"
        ["status", "--porcelain=v2", "--branch"]
        ""
    pure $ case exitCode of
        ExitSuccess   -> ToolSuccess $ T.pack stdout
        ExitFailure c -> ToolError $ T.pack $ "Git status failed (exit " <> show c <> "): " <> stderr


-- | Get git log
gitLog :: ExecutorContext -> Int -> IO (ToolResult Text)
gitLog ctx limit = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode
        "git"
        ["log", "--format=%H|%an|%s", "-" <> show limit]
        ""
    pure $ case exitCode of
        ExitSuccess   -> ToolSuccess $ T.pack stdout
        ExitFailure c -> ToolError $ T.pack $ "Git log failed (exit " <> show c <> "): " <> stderr


-- | Execute shell command
-- Respects approval policy for security
execCommand :: ExecutorContext -> Text -> IO (ToolResult Text)
execCommand ctx command = do
    -- Check approval policy
    case ctxApprovalPolicy ctx of
        Untrusted -> pure $ ToolError "Command execution requires approval in untrusted mode"
        _ -> do
            (exitCode, stdout, stderr) <- readProcessWithExitCode
                "sh"
                ["-c", T.unpack command]
                ""
            let output = T.pack $ stdout <> stderr
            pure $ case exitCode of
                ExitSuccess   -> ToolSuccess output
                ExitFailure c -> ToolError $ "Exit code " <> T.pack (show c) <> ": " <> output


-- | Tool argument types
data ToolArgs
    = ReadFileArgs { argPath :: FilePath }
    | ListDirArgs { argPath :: FilePath, argShowHidden :: Bool }
    | GitStatusArgs
    | GitLogArgs { argLimit :: Int }
    | ExecArgs { argCommand :: Text }
    deriving (Show, Eq)


-- | Execute a tool by name with arguments
execute :: ZapAgent -> Text -> ToolArgs -> IO (ToolResult Text)
execute agent "read_file" (ReadFileArgs path) =
    readFile' (agentCtx agent) path

execute agent "list_dir" (ListDirArgs path showHidden) = do
    result <- listDir (agentCtx agent) path showHidden
    pure $ fmap (T.pack . BL.unpack . encode) result

execute agent "git_status" GitStatusArgs =
    gitStatus (agentCtx agent)

execute agent "git_log" (GitLogArgs limit) =
    gitLog (agentCtx agent) limit

execute agent "exec" (ExecArgs command) =
    execCommand (agentCtx agent) command

execute _ name _ =
    pure $ ToolError $ "Unknown tool: " <> name


-- | Helper to format tool results for display
showResult :: ToolResult Text -> String
showResult (ToolSuccess content) =
    let preview = T.take 100 content
        suffix = if T.length content > 100 then "..." else ""
    in T.unpack $ preview <> suffix
showResult (ToolError e) = "Error: " <> T.unpack e


-- | Main entry point
main :: IO ()
main = do
    putStrLn "ZAP Haskell Agent Example"
    putStrLn "========================="
    putStrLn ""

    -- Create agent
    agent <- newZapAgent "."

    -- Display context info
    putStrLn $ "Session: " <> maybe "none" T.unpack (ctxSessionId $ agentCtx agent)
    putStrLn $ "Working directory: " <> ctxCwd (agentCtx agent)
    putStrLn ""

    -- List available tools
    putStrLn $ "Available tools (" <> show (length listTools) <> "):"
    mapM_ (\t -> putStrLn $ "  - " <> T.unpack t) listTools
    putStrLn ""

    -- Example 1: Read a file
    putStrLn "Example 1: Read file"
    result1 <- execute agent "read_file" (ReadFileArgs "Cargo.toml")
    case result1 of
        ToolSuccess content ->
            printf "  Result: %d bytes read\n" (T.length content)
        ToolError e ->
            putStrLn $ "  Error: " <> T.unpack e
    putStrLn ""

    -- Example 2: List directory
    putStrLn "Example 2: List directory"
    result2 <- execute agent "list_dir" (ListDirArgs "." False)
    putStrLn $ "  Result: " <> showResult result2
    putStrLn ""

    -- Example 3: Git status
    putStrLn "Example 3: Git status"
    result3 <- execute agent "git_status" GitStatusArgs
    putStrLn $ "  Result: " <> showResult result3
    putStrLn ""

    -- Example 4: Permission check (demonstrate approval policy)
    putStrLn "Example 4: Permission check"
    let ctx = agentCtx agent
    putStrLn $ "  Approval policy: " <> show (ctxApprovalPolicy ctx)
    putStrLn $ "  Sandbox policy: " <> case ctxSandboxPolicy ctx of
        DangerFullAccess -> "danger-full-access"
        ReadOnly -> "read-only"
        WorkspaceWrite{} -> "workspace-write"

    -- Example 5: Execute a command
    putStrLn ""
    putStrLn "Example 5: Execute command"
    result5 <- execute agent "exec" (ExecArgs "echo 'Hello from ZAP Haskell agent!'")
    putStrLn $ "  Result: " <> showResult result5
