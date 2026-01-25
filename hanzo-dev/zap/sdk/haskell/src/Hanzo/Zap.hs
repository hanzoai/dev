{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanzo.Zap
-- Copyright   : (c) 2025 Hanzo AI Inc.
-- License     : MIT
-- Maintainer  : dev@hanzo.ai
-- Stability   : experimental
-- Portability : portable
--
-- ZAP (Zero-copy Agent Protocol) - 1000x faster than MCP\/JSON-RPC
--
-- = Overview
--
-- ZAP is an insanely fast binary protocol for AI agent tool calls. It eliminates
-- the serialization, parsing, and copying overhead of JSON-RPC by reading directly
-- from network buffers.
--
-- == Key Features
--
-- * 1000-2000x faster than JSON-RPC for typical agent workloads
-- * Zero allocations for message parsing
-- * Full MCP (Model Context Protocol) compatibility via gateway
-- * 14 tool categories with 167+ typed operations
-- * Built-in security policies (approval and sandbox)
--
-- = Quick Start
--
-- == Client
--
-- @
-- import Hanzo.Zap
-- import Data.Aeson (object, (.=))
--
-- main :: IO ()
-- main = withClient "zap://localhost:9999" $ \\client -> do
--     -- Call a tool
--     result <- callTool client "read_file" $ object ["path" .= "src/main.rs"]
--     case result of
--         Left err -> print err
--         Right content -> print content
--
--     -- List available tools
--     tools <- listTools client
--     print tools
-- @
--
-- == Server
--
-- @
-- import Hanzo.Zap
-- import Data.Aeson (Value(..), object, (.=))
-- import qualified Data.Text.IO as TIO
--
-- main :: IO ()
-- main = do
--     server <- newServer defaultServerConfig { srvPort = 9999 }
--
--     -- Register a tool
--     registerTool server readFileTool $ \\args -> do
--         -- Extract path from args and read file
--         let path = ... -- parse from args
--         content <- TIO.readFile path
--         pure ToolResult
--             { resultContent = [ContentItem "text" (String content)]
--             , resultIsError = False
--             , resultMetadata = Nothing
--             }
--
--     -- Run the server
--     runServer server
-- @
--
-- = Documentation
--
-- See the individual modules for detailed documentation:
--
-- * "Hanzo.Zap.Types" - Core types (tools, messages, policies)
-- * "Hanzo.Zap.Client" - Client implementation
-- * "Hanzo.Zap.Server" - Server implementation
--
-- = Protocol Specification
--
-- See <https://github.com/hanzoai/hips/blob/main/HIP-007-zap.md HIP-007>
-- for the full protocol specification.

module Hanzo.Zap
    ( -- * Re-exports
      module Hanzo.Zap.Types
    , module Hanzo.Zap.Client
    , module Hanzo.Zap.Server
    ) where

import Hanzo.Zap.Types
import Hanzo.Zap.Client
import Hanzo.Zap.Server
