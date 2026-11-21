package frame

import (
	"fmt"
	"strings"
)

// NeedsSandbox is Handle's answer to a request that needs a workspace. The
// Adapter has sent nothing for it: the session layer either leases a cell and
// carries the request there, or calls Refuse.
type NeedsSandbox struct {
	// ID is the request's id, and Method its name.
	ID     int
	Method string
	// Scope is the environment scope the method asks for, which is how the
	// client names the same thing.
	Scope string
}

func (n *NeedsSandbox) Error() string {
	return fmt.Sprintf("frame: %s needs a sandbox", n.Method)
}

// Refuse answers a request with the typed error the client already
// understands for a method its session may not use: an
// EnvironmentAuthorizationError that names the scope and asks for the
// "sandbox" permission.
func (a *Adapter) Refuse(n *NeedsSandbox) error {
	return a.fail(n.ID, obj(
		"_tag", "EnvironmentAuthorizationError",
		"message", fmt.Sprintf("%s needs a sandbox; this session has no workspace.", n.Method),
		"requiredScope", n.Scope,
		"requiredPermission", "sandbox",
	))
}

// workspace is the methods a chat-mode session cannot answer, by prefix, and
// the scope each one is guarded by in the Dev server.
var workspace = []struct{ prefix, scope string }{
	{"terminal", "terminal:operate"},
	{"subscribeTerminal", "terminal:read"},
	{"subscribeDiscoveredLocalServers", "terminal:read"},
	{"preview", "preview:operate"},
	{"subscribePreview", "preview:operate"},
	{"projects.", "filesystem:read"},
	{"filesystem.", "filesystem:read"},
	{"vcs.", "source-control:write"},
	{"git.", "source-control:write"},
	{"subscribeVcs", "orchestration:read"},
	{"subscribeWorktreeSetup", "orchestration:read"},
	{"subscribeDeviceState", "orchestration:read"},
	{"device", "orchestration:operate"},
	{"browser", "preview:operate"},
	{"checkout", "source-control:write"},
}

// needsSandbox names a method that wants a workspace, or is nil.
func needsSandbox(method string) *NeedsSandbox {
	for _, w := range workspace {
		if strings.HasPrefix(method, w.prefix) {
			return &NeedsSandbox{Method: method, Scope: w.scope}
		}
	}
	return nil
}
