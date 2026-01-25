package zap

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"sync"
)

// ToolHandler is a function that handles tool calls.
type ToolHandler func(args map[string]any) (any, error)

// Server is a ZAP server for hosting tools.
type Server struct {
	name    string
	version string
	tools   map[string]struct {
		tool    Tool
		handler ToolHandler
	}
	mu       sync.RWMutex
	listener net.Listener
}

// NewServer creates a new ZAP server.
func NewServer(name, version string) *Server {
	return &Server{
		name:    name,
		version: version,
		tools: make(map[string]struct {
			tool    Tool
			handler ToolHandler
		}),
	}
}

// RegisterTool registers a tool with the server.
func (s *Server) RegisterTool(name, description string, inputSchema map[string]any, handler ToolHandler) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.tools[name] = struct {
		tool    Tool
		handler ToolHandler
	}{
		tool: Tool{
			Name:        name,
			Description: description,
			InputSchema: inputSchema,
		},
		handler: handler,
	}
}

// Listen starts the server on the specified port.
func (s *Server) Listen(port int) error {
	addr := fmt.Sprintf("0.0.0.0:%d", port)
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}

	s.listener = listener

	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
		}

		go s.handleConnection(conn)
	}
}

// Close stops the server.
func (s *Server) Close() error {
	if s.listener != nil {
		return s.listener.Close()
	}
	return nil
}

func (s *Server) handleConnection(conn net.Conn) {
	defer conn.Close()

	for {
		msgType, payload, err := s.recv(conn)
		if err != nil {
			return
		}

		s.handleMessage(conn, msgType, payload)
	}
}

func (s *Server) handleMessage(conn net.Conn, msgType MessageType, payload []byte) {
	switch msgType {
	case MsgInit:
		info := ServerInfo{
			Name:    s.name,
			Version: s.version,
		}
		info.Capabilities.Tools = true
		s.send(conn, MsgInitAck, info)

	case MsgListTools:
		s.mu.RLock()
		tools := make([]Tool, 0, len(s.tools))
		for _, t := range s.tools {
			tools = append(tools, t.tool)
		}
		s.mu.RUnlock()
		s.send(conn, MsgListToolsResponse, tools)

	case MsgCallTool:
		var call ToolCall
		if err := json.Unmarshal(payload, &call); err != nil {
			s.send(conn, MsgError, map[string]string{"message": err.Error()})
			return
		}

		result := s.executeTool(call)
		s.send(conn, MsgCallToolResponse, result)

	case MsgPing:
		s.send(conn, MsgPong, nil)

	default:
		s.send(conn, MsgError, map[string]string{"message": fmt.Sprintf("unknown message type: %d", msgType)})
	}
}

func (s *Server) executeTool(call ToolCall) ToolResult {
	s.mu.RLock()
	entry, ok := s.tools[call.Name]
	s.mu.RUnlock()

	if !ok {
		return ToolResult{
			ID:    call.ID,
			Error: fmt.Sprintf("unknown tool: %s", call.Name),
		}
	}

	content, err := entry.handler(call.Args)
	if err != nil {
		return ToolResult{
			ID:    call.ID,
			Error: err.Error(),
		}
	}

	return ToolResult{
		ID:      call.ID,
		Content: content,
	}
}

func (s *Server) send(conn net.Conn, msgType MessageType, payload any) error {
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

	if _, err := conn.Write(header); err != nil {
		return err
	}
	if len(payloadBytes) > 0 {
		if _, err := conn.Write(payloadBytes); err != nil {
			return err
		}
	}

	return nil
}

func (s *Server) recv(conn net.Conn) (MessageType, []byte, error) {
	header := make([]byte, 5)
	if _, err := io.ReadFull(conn, header); err != nil {
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
		if _, err := io.ReadFull(conn, payload); err != nil {
			return 0, nil, err
		}
	}

	return msgType, payload, nil
}
