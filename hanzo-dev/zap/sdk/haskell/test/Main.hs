{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Data.Aeson (encode, decode', object, (.=), Value(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

import Hanzo.Zap.Types

main :: IO ()
main = hspec $ do
    describe "Types" $ do
        describe "ToolCategory" $ do
            it "serializes to JSON correctly" $ do
                encode FileOps `shouldBe` "\"file\""
                encode GitOps `shouldBe` "\"git\""
                encode ExecOps `shouldBe` "\"exec\""

            it "deserializes from JSON correctly" $ do
                decode' "\"file\"" `shouldBe` Just FileOps
                decode' "\"git\"" `shouldBe` Just GitOps
                decode' "\"build\"" `shouldBe` Just BuildOps

        describe "Permission" $ do
            it "serializes to JSON correctly" $ do
                encode PermRead `shouldBe` "\"read\""
                encode PermWrite `shouldBe` "\"write\""
                encode PermExecute `shouldBe` "\"execute\""
                encode PermAdmin `shouldBe` "\"admin\""

        describe "ApprovalPolicy" $ do
            it "serializes to JSON correctly" $ do
                encode ApprovalNever `shouldBe` "\"never\""
                encode ApprovalOnFailure `shouldBe` "\"on-failure\""
                encode ApprovalOnRequest `shouldBe` "\"on-request\""
                encode ApprovalUnlessTrusted `shouldBe` "\"unless-trusted\""

            it "round-trips through JSON" $ do
                let policies = [minBound..maxBound] :: [ApprovalPolicy]
                mapM_ (\p -> decode' (encode p) `shouldBe` Just p) policies

        describe "SandboxPolicy" $ do
            it "serializes danger-full-access correctly" $ do
                encode SandboxDangerFullAccess `shouldBe` "\"danger-full-access\""

            it "serializes read-only correctly" $ do
                encode SandboxReadOnly `shouldBe` "\"read-only\""

            it "round-trips workspace-write through JSON" $ do
                let policy = SandboxWorkspaceWrite defaultSandboxConfig
                decode' (encode policy) `shouldBe` Just policy

        describe "ErrorCode" $ do
            it "converts to correct integer values" $ do
                errorCodeToInt ErrParseError `shouldBe` (-32700)
                errorCodeToInt ErrMethodNotFound `shouldBe` (-32601)
                errorCodeToInt ErrTimeout `shouldBe` (-32001)

            it "converts from integer values" $ do
                intToErrorCode (-32700) `shouldBe` Just ErrParseError
                intToErrorCode (-32601) `shouldBe` Just ErrMethodNotFound
                intToErrorCode 0 `shouldBe` Nothing

        describe "Request" $ do
            it "serializes correctly" $ do
                let req = Request
                        { reqId = "test-1"
                        , reqToolName = "read_file"
                        , reqArguments = object ["path" .= ("test.txt" :: String)]
                        , reqMetadata = Nothing
                        }
                let json = encode req
                decode' json `shouldBe` Just req

        describe "Response" $ do
            it "serializes success response correctly" $ do
                let resp = Response
                        { respId = "test-1"
                        , respContent = Just (String "file contents")
                        , respError = Nothing
                        , respMetadata = Nothing
                        }
                let json = encode resp
                decode' json `shouldBe` Just resp

            it "serializes error response correctly" $ do
                let resp = Response
                        { respId = "test-1"
                        , respContent = Nothing
                        , respError = Just ZapError
                            { errCode = ErrMethodNotFound
                            , errMessage = "Tool not found"
                            , errData = Nothing
                            }
                        , respMetadata = Nothing
                        }
                let json = encode resp
                decode' json `shouldBe` Just resp

        describe "Tool" $ do
            it "serializes and deserializes correctly" $ do
                let tool = Tool
                        { toolName = "read_file"
                        , toolDescription = "Read file contents"
                        , toolCategory = FileOps
                        , toolPermission = PermRead
                        , toolInputSchema = ToolSchema
                            { schemaType = "object"
                            , schemaRequired = ["path"]
                            , schemaProperties = Map.fromList
                                [("path", object ["type" .= ("string" :: String)])]
                            }
                        }
                decode' (encode tool) `shouldBe` Just tool

        describe "Capabilities" $ do
            it "serializes correctly" $ do
                let caps = Capabilities True True False True
                decode' (encode caps) `shouldBe` Just caps

        describe "InitMessage" $ do
            it "round-trips through JSON" $ do
                let msg = InitMessage "test-agent" "1.0.0"
                decode' (encode msg) `shouldBe` Just msg

        describe "InitAck" $ do
            it "round-trips through JSON" $ do
                let ack = InitAck
                        { ackName = "zap-server"
                        , ackVersion = "0.6.0"
                        , ackCapabilities = Capabilities True True True True
                        }
                decode' (encode ack) `shouldBe` Just ack

        describe "ToolResult" $ do
            it "serializes correctly" $ do
                let result = ToolResult
                        { resultContent =
                            [ ContentItem "text/plain" (String "Hello")
                            ]
                        , resultIsError = False
                        , resultMetadata = Nothing
                        }
                decode' (encode result) `shouldBe` Just result

        describe "Notification" $ do
            it "round-trips through JSON" $ do
                let notif = Notification "log" (Just $ object ["level" .= ("info" :: String)])
                decode' (encode notif) `shouldBe` Just notif
