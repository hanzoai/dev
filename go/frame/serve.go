package frame

import (
	"crypto/sha256"
	"encoding/base64"
	"errors"
	"fmt"
)

// The methods of a ZAP session, by the number its envelope carries. Method 0
// is one frame of the environment protocol, which is what Handle takes. The
// others are the three questions a client asks before it opens the stream, as
// the Dev server answers them over HTTP at /.well-known/hanzo/environment,
// /v1/auth/session and /v1/event/shell: here they are calls on the session, and
// there is no HTTP.
const (
	MethodFrame      = 0
	MethodDescriptor = 1
	MethodSession    = 2
	MethodShell      = 3
)

// ErrMethod is Call's answer to a method this Adapter does not serve.
var ErrMethod = errors.New("frame: unknown method")

// Call answers one of the session's non-frame methods with the JSON body the
// Dev server's HTTP route returns for it. payload is the route's query, as a
// JSON object, and is ignored: the snapshot is always whole.
func (a *Adapter) Call(method int, payload []byte) ([]byte, error) {
	switch method {
	case MethodDescriptor:
		return a.descriptor().MarshalJSON()
	case MethodSession:
		return a.sessionState().MarshalJSON()
	case MethodShell:
		b, _, err := a.Snapshot()
		return b, err
	}
	return nil, fmt.Errorf("%w %d", ErrMethod, method)
}

// Snapshot is the thread list as of its sequence: the body of the shell
// snapshot, and the sequence a socket opened after it resumes from.
func (a *Adapter) Snapshot() (shell []byte, sequence int64, err error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	s := a.shellBody(obj("schemaVersion", 2, "snapshotSequence", 0))
	// Nothing is older than the cut here: every thread is sent.
	s.Set("olderSettled", obj("before", a.now(), "asOfSequence", a.st.Seq, "counts", []any{}))
	id := sha256.Sum256([]byte(a.st.Project.Str("workspaceRoot")))
	s.Set("projectIdentities", base64.RawURLEncoding.EncodeToString(id[:]))
	b, err := s.MarshalJSON()
	return b, int64(a.st.Seq), err
}

func (a *Adapter) descriptor() *O {
	d := load("config").Obj("config").Obj("environment")
	a.environment(d)
	return d
}

func (a *Adapter) sessionState() *O {
	s := load("session")
	s.Set("sessionMethod", "bearer-access-token")
	if a.cfg.SessionExpires.IsZero() {
		s.Del("expiresAt")
	} else {
		s.Set("expiresAt", ts(a.cfg.SessionExpires))
	}
	return s
}
