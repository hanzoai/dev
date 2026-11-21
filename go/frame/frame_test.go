package frame

import (
	"bufio"
	"context"
	"errors"
	"flag"
	"fmt"
	"net/url"
	"os"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	dev "github.com/hanzoai/dev/go"
)

// corpus writes every frame of TestChat, in the form of testdata/chat.jsonl,
// for `make frames-check` to validate against the Dev contracts.
var corpus = flag.String("corpus", "", "write the frames TestChat sends and receives to this file")

// fx is a model that answers in two increments, and a ledger that keeps the
// last state it was handed.
type fx struct {
	mu    sync.Mutex
	asks  []dev.Ask
	saved [][]byte
	reply func(dev.Ask) (string, error)
}

func (f *fx) Model(_ context.Context, ask dev.Ask, delta func(string)) (dev.Reply, error) {
	f.mu.Lock()
	f.asks = append(f.asks, ask)
	f.mu.Unlock()
	text, err := f.reply(ask)
	if err != nil {
		return dev.Reply{}, err
	}
	if len(text) > 2 {
		delta(text[:len(text)/2])
		delta(text[len(text)/2:])
	}
	return dev.Reply{Text: &text}, nil
}

func (f *fx) Perform(context.Context, dev.Op) (dev.Output, error) {
	return dev.Output{}, &NeedsSandbox{Method: "exec"}
}

func (f *fx) Save(_ context.Context, state []byte) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.saved = append(f.saved, state)
	return nil
}

func greeting(ask dev.Ask) (string, error) {
	if ask.Prelude != nil && strings.HasPrefix(*ask.Prelude, "Reply with a title") {
		return `"One-Word Greeting."`, nil
	}
	last := ask.Messages[len(ask.Messages)-1]
	if last.User != nil && strings.Contains(*last.User, "French") {
		return "Bonjour", nil
	}
	return "Hello", nil
}

// rig is a client: it sends requests and keeps every frame the Adapter sends
// back, whole, as a client that joined the parts would read them.
type rig struct {
	t    *testing.T
	a    *Adapter
	fx   *fx
	mu   sync.Mutex
	in   []*O
	log  []logged
	step string
	id   int
	tick int

	pending string
	nextReq int
}

type logged struct {
	step, dir string
	f         *O
}

func newRig(t *testing.T, query string) *rig {
	t.Helper()
	ctx := context.Background()
	core, err := dev.Open(ctx)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { core.Close(ctx) })
	r := &rig{t: t, fx: &fx{reply: greeting}, step: "connect"}
	q, _ := url.ParseQuery(query)
	r.a, err = New(Config{
		Core: core, Effects: r.fx, Environment: Environment{ID: "ec08e842-07b8-4297-87c9-3b1f4a91df02", Label: "Hanzo Cloud"}, Query: q,
		Send: r.recv,
		Now: func() time.Time {
			r.mu.Lock()
			defer r.mu.Unlock()
			r.tick++
			return time.Date(2026, 11, 21, 9, 0, 0, 0, time.UTC).Add(time.Duration(r.tick) * 10 * time.Millisecond)
		},
		NewID: func() string {
			r.mu.Lock()
			defer r.mu.Unlock()
			r.id++
			return uuidFrom(fmt.Sprint("id", r.id))
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { r.a.Close(ctx) })
	return r
}

// joined is the client's joinSocketFrameParts.
func (r *rig) recv(b []byte) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := string(b)
	if strings.HasPrefix(s, part) {
		r.pending += s[len(part):]
		return nil
	}
	s = r.pending + s
	r.pending = ""
	v, err := parse([]byte(s))
	if err != nil {
		return err
	}
	f := v.(*O)
	r.in = append(r.in, f)
	r.log = append(r.log, logged{r.step, "in", f})
	return nil
}

func (r *rig) req(tag string, payload *O) int {
	r.t.Helper()
	r.mu.Lock()
	id := r.nextReq
	r.nextReq++
	r.mu.Unlock()
	f := obj("_tag", "Request", "id", id, "tag", tag, "payload", payload, "traceId", "00", "spanId", "00", "sampled", true, "headers", []any{})
	r.mu.Lock()
	r.log = append(r.log, logged{r.step, "out", f})
	r.mu.Unlock()
	b, _ := f.MarshalJSON()
	if err := r.a.Handle(context.Background(), b); err != nil {
		var n *NeedsSandbox
		if errors.As(err, &n) {
			if err := r.a.Refuse(n); err != nil {
				r.t.Fatal(err)
			}
			return id
		}
		r.t.Fatalf("%s: %v", tag, err)
	}
	return id
}

// frames are the frames for one request, in order.
func (r *rig) frames(id int) []*O {
	r.mu.Lock()
	defer r.mu.Unlock()
	var out []*O
	for _, f := range r.in {
		if n, _ := f.Int("requestId"); n == id && f.Str("_tag") != "" {
			out = append(out, f)
		}
	}
	return out
}

// values are the stream items of a request, flattened.
func (r *rig) values(id int) []any {
	var out []any
	for _, f := range r.frames(id) {
		if f.Str("_tag") == "Chunk" {
			for _, v := range f.List("values") {
				out = append(out, v)
			}
		}
	}
	return out
}

func (r *rig) exit(id int) *O {
	r.t.Helper()
	for _, f := range r.frames(id) {
		if f.Str("_tag") == "Exit" {
			return f.Obj("exit")
		}
	}
	r.t.Fatalf("request %d has no exit", id)
	return nil
}

func (r *rig) wait() { r.a.Wait() }

func kinds(vs []any) []string {
	var out []string
	for _, x := range vs {
		v, _ := x.(*O)
		k := v.Str("kind")
		if e := v.Obj("event"); e != nil {
			k += ":" + e.Str("type")
		}
		out = append(out, k)
	}
	return out
}

func (r *rig) chat() (thread string) {
	r.step = "connect"
	cfg := r.req("subscribeServerConfig", obj("environmentThemes", true, "usageLimitSources", true, "usageLimitsCommand", true, "workspaceSnapshotsOnDemand", true))
	r.req("subscribeServerLifecycle", obj())
	shell := r.req("orchestration.subscribeShell", obj("afterSequence", 0, "requestCompletionMarker", true))
	if got := kinds(r.values(shell)); !slices.Equal(got, []string{"snapshot", "synchronized"}) {
		r.t.Fatalf("shell: %v", got)
	}
	if got := r.values(cfg)[0].(*O).Str("type"); got != "snapshot" {
		r.t.Fatalf("config: %v", got)
	}
	r.step = "refreshProviders"
	r.req("server.refreshProviders", obj("instanceId", "hanzo", "cwd", scratch, "workspaceSnapshotsOnDemand", true, "usageLimitsCommand", true))
	r.req("orchestration.getArchivedShellSnapshot", obj())
	r.req("server.reportClientActivity", obj("environmentId", "ec08e842-07b8-4297-87c9-3b1f4a91df02", "clientId", "c", "clientKind", "web", "visible", true, "focused", true, "recentlyInteracted", true, "appState", "active", "scopes", []any{}, "ttlMs", 45000, "observedAt", "2026-11-21T09:00:00.000Z"))

	r.step = "launch"
	thread = "cf2007af-da22-4ed2-941d-a50899bb82fe"
	project := r.a.st.Project.Str("id")
	launch := r.req("orchestration.launchThread", obj(
		"commandId", "c1", "creationSource", "web", "threadId", thread, "projectId", project, "title", "Say hello", "generateTitle", true,
		"modelSelection", obj("instanceId", "hanzo", "model", "enso-auto"), "runtimeMode", "full-access", "interactionMode", "default",
		"workspaceStrategy", obj("type", "root"),
		"initialMessage", obj("messageId", "06f0952e-671b-4040-aa58-eb56666e2d12", "text", "Say hello in one word.", "attachments", []any{}),
	))
	if s := r.exit(launch).Str("_tag"); s != "Success" {
		r.t.Fatalf("launch: %v", r.exit(launch))
	}
	sub := r.req("orchestration.subscribeThread", obj("threadId", thread, "requestCompletionMarker", true))
	r.wait()
	_ = sub
	return thread
}

func TestChat(t *testing.T) {
	r := newRig(t, "frameParts=1")
	thread := r.chat()
	sub := 7 // the id the subscription was given

	got := kinds(r.values(sub))
	if got[0] != "snapshot" || got[1] != "synchronized" {
		t.Fatalf("thread stream opens with %v", got[:2])
	}
	// The subscription opened while the turn ran, so it hears the rest of it:
	// the reply, the run closing in the order the Dev server closes it, and the
	// title that is asked for last.
	var types []string
	for _, k := range got[2:] {
		types = append(types, strings.TrimPrefix(k, "event:"))
	}
	tail := []string{"provider-turn.updated", "run-attempt.updated", "provider-thread.updated", "run.updated", "node.updated", "thread.metadata-updated"}
	if len(types) < len(tail) || !slices.Equal(types[len(types)-len(tail):], tail) {
		t.Fatalf("turn events: %v", types)
	}

	// The reply is Hello, streamed as "He" then "Hello", and final.
	th := r.a.st.threads[thread]
	reply := find(th.Messages, replyMsgID(runID(thread, 1), 1))
	if reply == nil || reply.Str("text") != "Hello" || reply.Get("streaming") != false {
		t.Fatalf("reply: %v", reply)
	}
	if got := th.Shell.Str("status"); got != "completed" {
		t.Fatalf("status %q", got)
	}
	if got := th.Thread.Str("title"); got != "One-Word Greeting" {
		t.Fatalf("title %q", got)
	}

	r.step = "visit"
	r.req("orchestration.dispatchCommand", obj("type", "thread.visit", "commandId", "c2", "threadId", thread, "visitedAt", "2026-11-21T09:01:00.000Z", "opened", true))
	r.step = "message"
	r.req("orchestration.dispatchCommand", obj("type", "message.dispatch", "createdBy", "user", "creationSource", "web", "commandId", "c3", "threadId", thread,
		"messageId", "m2", "text", "And now in French, one word.", "attachments", []any{}, "titleSeed", "French",
		"modelSelection", obj("instanceId", "hanzo", "model", "enso-auto"), "deliveryIntent", "auto", "dispatchMode", obj("type", "start_immediately")))
	r.wait()
	if m := find(th.Messages, replyMsgID(runID(thread, 2), 2)); m == nil || m.Str("text") != "Bonjour" {
		t.Fatalf("second reply: %v", m)
	}
	r.step = "rename"
	r.req("orchestration.dispatchCommand", obj("type", "thread.metadata.update", "commandId", "c4", "threadId", thread, "title", "Hello Notes"))
	r.step = "settle"
	r.req("orchestration.dispatchCommand", obj("type", "thread.settle", "commandId", "c5", "threadId", thread))
	if th.Thread.Str("title") != "Hello Notes" || th.Thread.Str("settledOverride") != "settled" {
		t.Fatalf("thread: %v", th.Thread)
	}

	// The loop kept the conversation: its second ask carried the first turn.
	last := r.fx.asks[len(r.fx.asks)-1]
	if len(last.Messages) < 3 {
		t.Fatalf("the loop forgot the first turn: %d messages", len(last.Messages))
	}

	r.step = "bad"
	for m, payload := range map[string]*O{
		"vcs.createWorktree": obj("cwd", scratch, "refName", "main", "path", nil),
		"projects.writeFile": obj("cwd", scratch, "relativePath", "a", "contents", "b"),
		"terminal.open":      obj("threadId", thread, "terminalId", "t", "cwd", scratch),
	} {
		id := r.req(m, payload)
		e := r.exit(id)
		cause := e.List("cause")[0].(*O)
		fail := cause.Obj("error")
		if e.Str("_tag") != "Failure" || fail.Str("_tag") != "EnvironmentAuthorizationError" || fail.Str("requiredPermission") != "sandbox" {
			t.Fatalf("%s: %v", m, e)
		}
	}

	compare(t, r.log)
	// The read side of an empty workspace answers as the Dev server does.
	for m, payload := range map[string]*O{
		"vcs.listRefs":      obj("cwd", scratch, "limit", 100),
		"preview.list":      obj("threadId", thread),
		"projects.readFile": obj("cwd", scratch, "relativePath", "hanzo.json", "allowMissing", true),
	} {
		if e := r.exit(r.req(m, payload)); e.Str("_tag") != "Success" {
			t.Fatalf("%s: %v", m, e)
		}
	}
	for _, m := range []string{"subscribeVcsStatus", "subscribeVcsBranch", "subscribeTerminalMetadata", "subscribeProjectClones", "scheduledTasks.subscribe", "subscribeDiscoveredLocalServers"} {
		p := obj()
		if m == "subscribeVcsStatus" || m == "subscribeVcsBranch" {
			p.Set("cwd", scratch)
		}
		if len(r.values(r.req(m, p))) != 1 {
			t.Fatalf("%s did not answer", m)
		}
	}
	if *corpus != "" {
		write(t, *corpus, r.log)
	}
}

func write(t *testing.T, path string, log []logged) {
	t.Helper()
	f, err := os.Create(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	w := bufio.NewWriter(f)
	defer w.Flush()
	for _, l := range log {
		b, _ := obj("step", l.step, "dir", l.dir, "f", l.f).MarshalJSON()
		w.Write(b)
		w.WriteByte('\n')
	}
}

// A frame larger than a part is split on the way out and joins back whole.
func TestParts(t *testing.T) {
	big := strings.Repeat("é", partBytes) // two bytes each
	parts := split(big)
	if len(parts) < 2 {
		t.Fatalf("%d parts", len(parts))
	}
	var joined strings.Builder
	for i, p := range parts {
		if len(p) > partBytes {
			t.Fatalf("part %d is %d bytes", i, len(p))
		}
		if i < len(parts)-1 {
			if !strings.HasPrefix(p, part) {
				t.Fatalf("part %d lacks the mark", i)
			}
			p = p[len(part):]
		}
		joined.WriteString(p)
	}
	if joined.String() != big {
		t.Fatal("parts do not join to the frame")
	}
}

// The state a session keeps restores to the same conversation.
func TestRestore(t *testing.T) {
	r := newRig(t, "")
	thread := r.chat()
	state, err := r.a.State()
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	core, err := dev.Open(ctx)
	if err != nil {
		t.Fatal(err)
	}
	defer core.Close(ctx)
	r2 := &rig{t: t, fx: &fx{reply: greeting}, step: "connect"}
	r2.a, err = New(Config{Core: core, Effects: r2.fx, State: state, Send: r2.recv, Environment: Environment{ID: "e"}})
	if err != nil {
		t.Fatal(err)
	}
	defer r2.a.Close(ctx)
	r2.req("orchestration.dispatchCommand", obj("type", "message.dispatch", "commandId", "c", "threadId", thread, "messageId", "m", "text", "And now in French, one word.", "attachments", []any{},
		"modelSelection", obj("instanceId", "hanzo", "model", "enso-auto")))
	r2.wait()
	last := r2.fx.asks[len(r2.fx.asks)-1]
	if len(last.Messages) < 3 {
		t.Fatalf("a restored session forgot the first turn: %d messages", len(last.Messages))
	}
}
