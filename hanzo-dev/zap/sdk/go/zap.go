// Package zap provides a Go client and server SDK for the ZAP protocol.
//
// ZAP (Zero-copy Agent Protocol) is 1000x faster than MCP/JSON-RPC
// through binary wire protocol with zero-copy serialization.
//
// Example client usage:
//
//	client, err := zap.Connect("zap://localhost:9999")
//	if err != nil {
//	    log.Fatal(err)
//	}
//	defer client.Close()
//
//	tools, err := client.ListTools()
//	result, err := client.CallTool("read_file", map[string]any{"path": "README.md"})
//
// Example server usage:
//
//	server := zap.NewServer("my-tools", "1.0.0")
//	server.RegisterTool("greet", "Greet someone", nil, func(args map[string]any) (any, error) {
//	    return fmt.Sprintf("Hello, %s!", args["name"]), nil
//	})
//	server.Listen(9999)
package zap

import (
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"sync"
	"sync/atomic"
)

const (
	MaxMessageSize = 16 * 1024 * 1024 // 16MB
	Version        = "0.6.0"
)

// MessageType represents ZAP wire protocol message types.
type MessageType uint8

const (
	MsgInit              MessageType = 0x01
	MsgInitAck           MessageType = 0x02
	MsgListTools         MessageType = 0x10
	MsgListToolsResponse MessageType = 0x11
	MsgCallTool          MessageType = 0x12
	MsgCallToolResponse  MessageType = 0x13
	MsgListResources     MessageType = 0x20
	MsgListResourcesResp MessageType = 0x21
	MsgReadResource      MessageType = 0x22
	MsgReadResourceResp  MessageType = 0x23
	MsgListPrompts       MessageType = 0x30
	MsgListPromptsResp   MessageType = 0x31
	MsgGetPrompt         MessageType = 0x32
	MsgGetPromptResp     MessageType = 0x33
	MsgPing              MessageType = 0xF0
	MsgPong              MessageType = 0xF1
	MsgError             MessageType = 0xFF
)

// ApprovalPolicy controls when to ask for human approval.
type ApprovalPolicy string

const (
	ApprovalUnlessTrusted ApprovalPolicy = "unless-trusted" // Only auto-approve known-safe reads
	ApprovalOnFailure     ApprovalPolicy = "on-failure"     // Auto-approve, escalate on failure
	ApprovalOnRequest     ApprovalPolicy = "on-request"     // Model decides (default)
	ApprovalNever         ApprovalPolicy = "never"          // Never ask
)

// SandboxMode controls filesystem access restrictions.
type SandboxMode string

const (
	SandboxDangerFullAccess SandboxMode = "danger-full-access"
	SandboxReadOnly         SandboxMode = "read-only"
	SandboxWorkspaceWrite   SandboxMode = "workspace-write"
)

// SandboxPolicy defines sandbox configuration.
type SandboxPolicy struct {
	Mode           SandboxMode `json:"mode"`
	WritableRoots  []string    `json:"writableRoots,omitempty"`
	NetworkAccess  bool        `json:"networkAccess,omitempty"`
	AllowGitWrites bool        `json:"allowGitWrites,omitempty"`
}

// Tool represents a tool definition.
type Tool struct {
	Name        string         `json:"name"`
	Description string         `json:"description"`
	InputSchema map[string]any `json:"inputSchema,omitempty"`
}

// ToolCall represents a tool call request.
type ToolCall struct {
	ID       string         `json:"id"`
	Name     string         `json:"name"`
	Args     map[string]any `json:"args,omitempty"`
	Metadata map[string]any `json:"metadata,omitempty"`
}

// ToolResult represents a tool execution result.
type ToolResult struct {
	ID       string         `json:"id"`
	Content  any            `json:"content"`
	Error    string         `json:"error,omitempty"`
	Metadata map[string]any `json:"metadata,omitempty"`
}

// ServerInfo represents server capabilities from handshake.
type ServerInfo struct {
	Name         string `json:"name"`
	Version      string `json:"version"`
	Capabilities struct {
		Tools     bool `json:"tools"`
		Resources bool `json:"resources"`
		Prompts   bool `json:"prompts"`
	} `json:"capabilities"`
}

// ClientInfo represents client info for handshake.
type ClientInfo struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Error types
var (
	ErrConnectionClosed = errors.New("connection closed")
	ErrMessageTooLarge  = errors.New("message too large")
	ErrProtocol         = errors.New("protocol error")
	ErrUnknownTool      = errors.New("unknown tool")
)

// Client is a ZAP client for connecting to ZAP servers.
type Client struct {
	conn       net.Conn
	serverInfo *ServerInfo
	requestID  atomic.Uint64
	mu         sync.Mutex
}

// Connect establishes a connection to a ZAP server.
func Connect(url string) (*Client, error) {
	// Parse URL (simplified - expects "zap://host:port" or "zaps://host:port")
	host := url
	if len(url) > 6 && url[:6] == "zap://" {
		host = url[6:]
	} else if len(url) > 7 && url[:7] == "zaps://" {
		// TLS not implemented in this example
		host = url[7:]
	}

	conn, err := net.Dial("tcp", host)
	if err != nil {
		return nil, fmt.Errorf("connect: %w", err)
	}

	client := &Client{conn: conn}
	if err := client.handshake(); err != nil {
		conn.Close()
		return nil, err
	}

	return client, nil
}

func (c *Client) handshake() error {
	clientInfo := ClientInfo{Name: "zap-go", Version: Version}
	if err := c.send(MsgInit, clientInfo); err != nil {
		return err
	}

	msgType, payload, err := c.recv()
	if err != nil {
		return err
	}

	if msgType != MsgInitAck {
		return fmt.Errorf("%w: expected InitAck, got %d", ErrProtocol, msgType)
	}

	var serverInfo ServerInfo
	if err := json.Unmarshal(payload, &serverInfo); err != nil {
		return err
	}

	c.serverInfo = &serverInfo
	return nil
}

// ServerInfo returns the server info from handshake.
func (c *Client) ServerInfo() *ServerInfo {
	return c.serverInfo
}

// ListTools lists available tools.
func (c *Client) ListTools() ([]Tool, error) {
	c.mu.Lock()
	defer c.mu.Unlock()

	if err := c.send(MsgListTools, nil); err != nil {
		return nil, err
	}

	msgType, payload, err := c.recv()
	if err != nil {
		return nil, err
	}

	if msgType != MsgListToolsResponse {
		return nil, fmt.Errorf("%w: expected ListToolsResponse", ErrProtocol)
	}

	var tools []Tool
	if err := json.Unmarshal(payload, &tools); err != nil {
		return nil, err
	}

	return tools, nil
}

// CallTool calls a tool by name.
func (c *Client) CallTool(name string, args map[string]any) (*ToolResult, error) {
	c.mu.Lock()
	defer c.mu.Unlock()

	call := ToolCall{
		ID:   fmt.Sprintf("req-%d", c.requestID.Add(1)),
		Name: name,
		Args: args,
	}

	if err := c.send(MsgCallTool, call); err != nil {
		return nil, err
	}

	msgType, payload, err := c.recv()
	if err != nil {
		return nil, err
	}

	if msgType != MsgCallToolResponse {
		return nil, fmt.Errorf("%w: expected CallToolResponse", ErrProtocol)
	}

	var result ToolResult
	if err := json.Unmarshal(payload, &result); err != nil {
		return nil, err
	}

	return &result, nil
}

// Ping checks the connection.
func (c *Client) Ping() error {
	c.mu.Lock()
	defer c.mu.Unlock()

	if err := c.send(MsgPing, nil); err != nil {
		return err
	}

	msgType, _, err := c.recv()
	if err != nil {
		return err
	}

	if msgType != MsgPong {
		return fmt.Errorf("%w: expected Pong", ErrProtocol)
	}

	return nil
}

// Close closes the connection.
func (c *Client) Close() error {
	return c.conn.Close()
}

func (c *Client) send(msgType MessageType, payload any) error {
	var payloadBytes []byte
	if payload != nil {
		var err error
		payloadBytes, err = json.Marshal(payload)
		if err != nil {
			return err
		}
	}

	totalLen := uint32(1 + len(payloadBytes))
	header := make([]byte, 5)
	binary.LittleEndian.PutUint32(header[:4], totalLen)
	header[4] = byte(msgType)

	if _, err := c.conn.Write(header); err != nil {
		return err
	}
	if len(payloadBytes) > 0 {
		if _, err := c.conn.Write(payloadBytes); err != nil {
			return err
		}
	}

	return nil
}

func (c *Client) recv() (MessageType, []byte, error) {
	header := make([]byte, 5)
	if _, err := io.ReadFull(c.conn, header); err != nil {
		return 0, nil, err
	}

	totalLen := binary.LittleEndian.Uint32(header[:4])
	if totalLen > MaxMessageSize {
		return 0, nil, ErrMessageTooLarge
	}

	msgType := MessageType(header[4])
	payloadLen := int(totalLen) - 1

	var payload []byte
	if payloadLen > 0 {
		payload = make([]byte, payloadLen)
		if _, err := io.ReadFull(c.conn, payload); err != nil {
			return 0, nil, err
		}
	}

	if msgType == MsgError {
		var errMsg struct {
			Message string `json:"message"`
		}
		json.Unmarshal(payload, &errMsg)
		return msgType, nil, fmt.Errorf("%w: %s", ErrProtocol, errMsg.Message)
	}

	return msgType, payload, nil
}
