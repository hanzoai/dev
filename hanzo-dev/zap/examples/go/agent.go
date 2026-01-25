// ZAP Agent Example - Go
//
// Demonstrates a complete ZAP agent with tool execution in Go.
//
// Usage:
//   go run agent.go

package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// AskForApproval determines when to ask for user approval
type AskForApproval string

const (
	ApprovalUntrusted AskForApproval = "untrusted"
	ApprovalOnFailure AskForApproval = "on-failure"
	ApprovalOnRequest AskForApproval = "on-request"
	ApprovalNever     AskForApproval = "never"
)

// SandboxMode determines execution restrictions
type SandboxMode string

const (
	SandboxDangerFullAccess SandboxMode = "danger-full-access"
	SandboxReadOnly         SandboxMode = "read-only"
	SandboxWorkspaceWrite   SandboxMode = "workspace-write"
)

// ExecutorContext contains execution state
type ExecutorContext struct {
	Cwd            string
	Env            map[string]string
	SessionID      string
	ApprovalPolicy AskForApproval
	SandboxMode    SandboxMode
	WritableRoots  []string
	NetworkAccess  bool
	TimeoutMs      int64
}

// ToolResult represents a tool execution result
type ToolResult struct {
	Content interface{} `json:"content"`
	Error   string      `json:"error,omitempty"`
}

// ZapAgent is a ZAP-compatible agent
type ZapAgent struct {
	ctx ExecutorContext
}

// NewZapAgent creates a new agent
func NewZapAgent(cwd string) *ZapAgent {
	absPath, _ := filepath.Abs(cwd)
	return &ZapAgent{
		ctx: ExecutorContext{
			Cwd:            absPath,
			Env:            envToMap(),
			SessionID:      fmt.Sprintf("go-agent-%d", os.Getpid()),
			ApprovalPolicy: ApprovalOnRequest,
			SandboxMode:    SandboxWorkspaceWrite,
			WritableRoots:  []string{},
			NetworkAccess:  true,
			TimeoutMs:      30000,
		},
	}
}

func envToMap() map[string]string {
	env := make(map[string]string)
	for _, e := range os.Environ() {
		parts := strings.SplitN(e, "=", 2)
		if len(parts) == 2 {
			env[parts[0]] = parts[1]
		}
	}
	return env
}

// Execute runs a tool by name
func (a *ZapAgent) Execute(name string, args map[string]interface{}) ToolResult {
	switch name {
	case "read_file":
		return a.readFile(args["path"].(string))
	case "list_dir":
		return a.listDir(args["path"].(string))
	case "git_status":
		return a.gitStatus()
	case "git_log":
		limit := 10
		if l, ok := args["limit"].(float64); ok {
			limit = int(l)
		}
		return a.gitLog(limit)
	case "exec":
		return a.exec(args["command"].(string))
	default:
		return ToolResult{Error: fmt.Sprintf("Unknown tool: %s", name)}
	}
}

func (a *ZapAgent) readFile(path string) ToolResult {
	fullPath := filepath.Join(a.ctx.Cwd, path)
	content, err := ioutil.ReadFile(fullPath)
	if err != nil {
		return ToolResult{Error: err.Error()}
	}
	return ToolResult{Content: string(content)}
}

func (a *ZapAgent) listDir(path string) ToolResult {
	fullPath := filepath.Join(a.ctx.Cwd, path)
	entries, err := ioutil.ReadDir(fullPath)
	if err != nil {
		return ToolResult{Error: err.Error()}
	}

	result := make([]map[string]interface{}, 0, len(entries))
	for _, e := range entries {
		result = append(result, map[string]interface{}{
			"name":  e.Name(),
			"isDir": e.IsDir(),
			"size":  e.Size(),
		})
	}
	return ToolResult{Content: result}
}

func (a *ZapAgent) gitStatus() ToolResult {
	cmd := exec.Command("git", "status", "--porcelain=v2", "--branch")
	cmd.Dir = a.ctx.Cwd
	output, err := cmd.Output()
	if err != nil {
		return ToolResult{Error: err.Error()}
	}
	return ToolResult{Content: string(output)}
}

func (a *ZapAgent) gitLog(limit int) ToolResult {
	cmd := exec.Command("git", "log", fmt.Sprintf("--format=%%H|%%an|%%s"), fmt.Sprintf("-%d", limit))
	cmd.Dir = a.ctx.Cwd
	output, err := cmd.Output()
	if err != nil {
		return ToolResult{Error: err.Error()}
	}
	return ToolResult{Content: string(output)}
}

func (a *ZapAgent) exec(command string) ToolResult {
	// Check approval policy
	if a.ctx.ApprovalPolicy == ApprovalUntrusted {
		return ToolResult{Error: "Command execution requires approval in untrusted mode"}
	}

	cmd := exec.Command("sh", "-c", command)
	cmd.Dir = a.ctx.Cwd
	output, err := cmd.CombinedOutput()
	if err != nil {
		return ToolResult{Content: string(output), Error: err.Error()}
	}
	return ToolResult{Content: string(output)}
}

// ListTools returns available tools
func (a *ZapAgent) ListTools() []string {
	return []string{
		"read_file",
		"list_dir",
		"git_status",
		"git_log",
		"exec",
	}
}

func main() {
	fmt.Println("ZAP Go Agent Example")
	fmt.Println("====================")
	fmt.Println()

	agent := NewZapAgent(".")

	// List available tools
	tools := agent.ListTools()
	fmt.Printf("Available tools (%d):\n", len(tools))
	for _, t := range tools {
		fmt.Printf("  - %s\n", t)
	}
	fmt.Println()

	// Example 1: Read a file
	fmt.Println("Example 1: Read file")
	result := agent.Execute("read_file", map[string]interface{}{"path": "Cargo.toml"})
	if result.Error != "" {
		fmt.Printf("  Error: %s\n", result.Error)
	} else {
		content := result.Content.(string)
		fmt.Printf("  Result: %d bytes read\n", len(content))
	}
	fmt.Println()

	// Example 2: List directory
	fmt.Println("Example 2: List directory")
	result = agent.Execute("list_dir", map[string]interface{}{"path": "."})
	if result.Error != "" {
		fmt.Printf("  Error: %s\n", result.Error)
	} else {
		jsonBytes, _ := json.Marshal(result.Content)
		preview := string(jsonBytes)
		if len(preview) > 100 {
			preview = preview[:100] + "..."
		}
		fmt.Printf("  Result: %s\n", preview)
	}
	fmt.Println()

	// Example 3: Git status
	fmt.Println("Example 3: Git status")
	result = agent.Execute("git_status", map[string]interface{}{})
	if result.Error != "" {
		fmt.Printf("  Error: %s\n", result.Error)
	} else {
		content := result.Content.(string)
		preview := content
		if len(preview) > 100 {
			preview = preview[:100] + "..."
		}
		fmt.Printf("  Result: %s\n", preview)
	}
}
