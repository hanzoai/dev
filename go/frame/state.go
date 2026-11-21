package frame

import (
	"crypto/sha256"
	"embed"
	json "encoding/json/v2"
	"fmt"
	"strings"
	"sync"

	dev "github.com/hanzo-inc/dev/go"
)

// tpl holds one recorded frame or entity for every shape the Adapter writes.
// Each came from a real Dev server in chat mode, with the paths and names of
// the machine it ran on replaced, and the Adapter fills in what is its own:
// ids, text, times and statuses. A member the Adapter does not set keeps the
// value the server gave it, so a frame has the members a real one has.
//
//go:embed tpl/*.json
var tpl embed.FS

// load is a fresh copy of a template.
func load(name string) *O {
	b, err := tpl.ReadFile("tpl/" + name + ".json")
	if err != nil {
		panic(err) // a template the package embeds is missing: a build defect
	}
	v, err := parse(b)
	if err != nil {
		panic(err)
	}
	return v.(*O)
}

// scratch is the working directory of a chat thread, which has no workspace of
// its own: the Dev server gives each such thread a directory under this one,
// and the client shows none of them.
const scratch = "/scratch"

// state is the Adapter's memory.
type state struct {
	Env     Environment `json:"env"`
	Seq     int         `json:"seq"`
	Project *O          `json:"project"`
	Threads []*thread   `json:"threads"`

	threads map[string]*thread
}

// thread is one conversation: its projection as the client reads it, and the
// loop that holds its history.
type thread struct {
	ID       string `json:"id"`
	Thread   *O     `json:"thread"`
	Shell    *O     `json:"shell"`
	Runs     []*O   `json:"runs"`
	Attempts []*O   `json:"attempts"`
	Nodes    []*O   `json:"nodes"`
	PThreads []*O   `json:"providerThreads"`
	PTurns   []*O   `json:"providerTurns"`
	Messages []*O   `json:"messages"`
	Items    []*O   `json:"items"`
	// Replies counts the assistant messages, which name the next one.
	Replies int `json:"replies"`
	// Core is the loop's snapshot as of its last Save.
	Core []byte `json:"core"`
	// Error is why the latest run failed, when it did.
	Error string `json:"error"`
	// Regenerate is the title request not yet answered.
	Regenerate string `json:"regenerate"`

	sess *dev.Session
	turn sync.Mutex
}

func newState(cfg Config) (*state, error) {
	if cfg.State != nil {
		st := &state{}
		if err := json.Unmarshal(cfg.State, st); err != nil {
			return nil, fmt.Errorf("frame: restoring state: %w", err)
		}
		st.threads = map[string]*thread{}
		for _, th := range st.Threads {
			st.threads[th.ID] = th
		}
		return st, nil
	}
	now := ts(cfg.Now())
	project := load("project")
	project.Set("id", firstNonEmpty(cfg.Project, uuidFrom("project:"+cfg.Environment.ID)))
	project.Set("workspaceRoot", scratch)
	project.Set("createdAt", now).Set("updatedAt", now)
	return &state{Env: cfg.Environment, Seq: cfg.Sequence, Project: project, threads: map[string]*thread{}}, nil
}

func (s *state) marshal() ([]byte, error) { return json.Marshal(s) }

// uuidFrom is a UUID that depends only on s.
func uuidFrom(s string) string {
	h := sha256.Sum256([]byte(s))
	h[6], h[8] = h[6]&0x0f|0x50, h[8]&0x3f|0x80
	return fmt.Sprintf("%x-%x-%x-%x-%x", h[0:4], h[4:6], h[6:8], h[8:10], h[10:16])
}

// esc is encodeURIComponent for the ids the Dev server derives from other ids.
func esc(s string) string {
	return strings.NewReplacer(":", "%3A", "/", "%2F").Replace(s)
}

// The ids of a run's entities are derived from the thread's and the run's
// ordinal, as the Dev server derives them, so a replay names the same ones.
func runID(tid string, n int) string       { return fmt.Sprintf("run:thread:%s:ordinal:%d", tid, n) }
func attemptID(rid string) string          { return "run-attempt:run:" + esc(rid) + ":attempt:1" }
func rootID(rid string) string             { return "node:run:" + esc(rid) + ":root" }
func pthreadID(tid string) string          { return "provider-thread:provider:hanzo:native-thread:" + esc(tid) }
func pturnID(rid string) string            { return "provider-turn:provider:hanzo:native-turn:" + esc(rid) }
func replyNative(rid string, k int) string { return "msg_" + esc(rid) + "-" + fmt.Sprint(k) }
func replyNodeID(rid string, k int) string {
	return "node:provider:hanzo:native-item:" + replyNative(rid, k)
}
func replyMsgID(rid string, k int) string {
	return "message:provider:hanzo:native-item:" + replyNative(rid, k)
}
func replyItemID(rid string, k int) string {
	return "turn-item:provider:hanzo:native-item:" + replyNative(rid, k)
}
func userItemID(mid string) string { return "turn-item:message:" + mid }

// up replaces the entity with the same id, or appends it.
func up(list *[]*O, o *O) {
	for i, x := range *list {
		if x.Str("id") == o.Str("id") {
			(*list)[i] = o
			return
		}
	}
	*list = append(*list, o)
}

func find(list []*O, id string) *O {
	for _, x := range list {
		if x.Str("id") == id {
			return x
		}
	}
	return nil
}
