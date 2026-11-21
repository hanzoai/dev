// Package frame is the Dev environment protocol on top of the dev loop.
//
// Hanzo Dev's client speaks Effect RPC: JSON frames, one request per call and
// a stream of chunks per subscription, with the orchestration-v2 schemas in
// the Dev app's packages/contracts. The loop in this repository speaks its own
// alphabet, eight events in and ten actions out. An Adapter is where the two
// meet (HIP-1330): it takes each frame a client sends, drives the loop with
// the events it means, and writes back the frames the real Dev server would
// have written for the same thing. The loop, the client and the server are
// unchanged.
//
// An Adapter serves chat mode: a thread with no workspace. The methods it
// answers are the chat-mode subset (HIP-1330): connecting, listing threads,
// launching one, sending a message, receiving the reply, renaming and
// settling. A method that needs a workspace (a shell, files, a checkout, a
// browser, a preview) is not answered; Handle returns a *NeedsSandbox naming
// it, and whoever owns the session decides: lease a cell and carry the request
// there, or Refuse it with the typed error the client already understands.
//
// The Adapter does no I/O of its own. The model, the persistence of its state
// and any effect a loop asks for are an Effects, supplied by the session layer
// that owns the lease, the ledger and the sandbox.
package frame

import (
	"context"
	"crypto/rand"
	"errors"
	"fmt"
	"net/url"
	"sync"
	"time"
	"unicode/utf8"

	dev "github.com/hanzoai/dev/go"
)

// Effects is what the Adapter asks of the session layer.
type Effects interface {
	// Model answers one ask, routed through Hanzo. Each increment of the reply
	// as it arrives goes to delta, so a client sees the text stream; the Reply
	// is the whole of it.
	Model(ctx context.Context, ask dev.Ask, delta func(string)) (dev.Reply, error)
	// Perform runs an Op the loop asked for that touches a workspace: Read,
	// Write, Patch, Exec, Git or Browse. A chat-mode session answers
	// *NeedsSandbox, and the session layer leases a cell and runs it there.
	Perform(ctx context.Context, op dev.Op) (dev.Output, error)
	// Save keeps the Adapter's State, which holds each thread's loop snapshot,
	// at the place in the sequence the loop asked for it.
	Save(ctx context.Context, state []byte) error
}

// Environment names the server the client believes it is talking to.
type Environment struct {
	ID    string
	Label string
}

// Config is what an Adapter is born with.
type Config struct {
	// Core runs the loop. One Core serves every Adapter in the process.
	Core    *dev.Core
	Effects Effects
	// Send writes one logical frame to the client. The Adapter calls it in
	// order, one call at a time, and splits a large frame into parts first when
	// the client asked.
	Send        func([]byte) error
	Environment Environment
	// Project is the id of the one project a chat session has, the scratch
	// space its threads live in. It is derived from the environment when empty.
	Project string
	// Sequence is where a new session's sequence of events starts: the
	// sequence of the snapshot the client already holds.
	Sequence int
	// Query is the query string the client opened its socket with.
	Query url.Values
	// State restores what a previous Adapter's State returned.
	State []byte
	// Now and NewID are the clock and the source of ids; tests replace them.
	Now   func() time.Time
	NewID func() string
}

// Adapter is one client's environment protocol, over one session.
type Adapter struct {
	cfg   Config
	parts bool

	mu   sync.Mutex
	st   *state
	subs map[int]*sub

	send sync.Mutex
	wg   sync.WaitGroup

	// bg is the context a turn runs under: it outlives the request that
	// started it, and Close ends it.
	bg     context.Context
	cancel context.CancelFunc
}

type subKind int

const (
	subConfig subKind = iota
	subLifecycle
	subShell
	subThread
	subStatic
)

type sub struct {
	kind   subKind
	thread string
}

// New starts an Adapter.
func New(cfg Config) (*Adapter, error) {
	if cfg.Core == nil || cfg.Effects == nil || cfg.Send == nil {
		return nil, errors.New("frame: Config needs a Core, Effects and Send")
	}
	if cfg.Now == nil {
		cfg.Now = time.Now
	}
	if cfg.NewID == nil {
		cfg.NewID = uuid
	}
	a := &Adapter{cfg: cfg, subs: map[int]*sub{}, parts: cfg.Query.Get("frameParts") == "1"}
	st, err := newState(cfg)
	if err != nil {
		return nil, err
	}
	a.st = st
	a.bg, a.cancel = context.WithCancel(context.Background())
	return a, nil
}

// Handle takes one frame from the client. It returns once the frames the
// request asks for immediately are sent; a reply that takes the model's time
// arrives later, through Send.
//
// A method that needs a workspace answers *NeedsSandbox and sends nothing.
func (a *Adapter) Handle(ctx context.Context, frame []byte) error {
	v, err := parse(frame)
	if err != nil {
		return fmt.Errorf("frame: not JSON: %w", err)
	}
	f, ok := v.(*O)
	if !ok {
		return errors.New("frame: a frame is an object")
	}
	switch f.Str("_tag") {
	case "Request":
		n, ok := f.Int("id")
		if !ok {
			return errors.New("frame: a request has a numeric id")
		}
		p, _ := f.Get("payload").(*O)
		if p == nil {
			p = newO()
		}
		return a.request(ctx, n, f.Str("tag"), p)
	case "Interrupt":
		n, _ := f.Int("requestId")
		return a.interrupt(n)
	case "Ping":
		return a.write(obj("_tag", "Pong"))
	case "Ack", "Eof", "Pong":
		return nil
	}
	return fmt.Errorf("frame: unknown frame %q", f.Str("_tag"))
}

// Wait returns when every turn that is running has finished.
func (a *Adapter) Wait() { a.wg.Wait() }

// State is everything the Adapter keeps between frames: the threads, their
// projections, and each loop's snapshot. Restore it with Config.State.
func (a *Adapter) State() ([]byte, error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.st.marshal()
}

// Close cancels the turns that are running, waits for them, and closes the
// loops.
func (a *Adapter) Close(ctx context.Context) error {
	a.cancel()
	a.Wait()
	a.mu.Lock()
	defer a.mu.Unlock()
	var errs []error
	for _, th := range a.st.threads {
		if th.sess != nil {
			errs = append(errs, th.sess.Close(ctx))
			th.sess = nil
		}
	}
	return errors.Join(errs...)
}

// request routes one request to the method that answers it.
func (a *Adapter) request(ctx context.Context, id int, tag string, p *O) error {
	switch tag {
	case "subscribeServerConfig":
		return a.serverConfig(id)
	case "subscribeServerLifecycle":
		return a.lifecycle(id)
	case "orchestration.subscribeShell":
		return a.subscribeShell(id, p)
	case "orchestration.subscribeThread":
		return a.subscribeThread(id, p)
	case "orchestration.launchThread":
		return a.launchThread(ctx, id, p)
	case "orchestration.dispatchCommand":
		return a.dispatch(ctx, id, p)
	case "orchestration.getArchivedShellSnapshot":
		return a.archived(id)
	case "server.refreshProviders":
		return a.refreshProviders(id)
	case "server.reportClientActivity":
		return a.exit(id, nil)
	case "peerLinks.list":
		return a.exit(id, obj("links", []any{}))
	}
	if r, ok := a.empty(id, tag, p); ok {
		return r
	}
	if n := needsSandbox(tag); n != nil {
		n.ID = id
		return n
	}
	return a.fail(id, obj(
		"_tag", "RpcDefect",
		"message", fmt.Sprintf("%s is not part of the chat-mode subset", tag),
	))
}

// interrupt ends a subscription.
func (a *Adapter) interrupt(id int) error {
	a.mu.Lock()
	_, ok := a.subs[id]
	delete(a.subs, id)
	a.mu.Unlock()
	if !ok {
		return nil
	}
	return a.write(obj("_tag", "Exit", "requestId", id, "exit", obj(
		"_tag", "Failure",
		"cause", []any{obj("_tag", "Interrupt", "fiberId", 1)},
	)))
}

// exit answers a request with its value.
func (a *Adapter) exit(id int, value any) error {
	return a.write(obj("_tag", "Exit", "requestId", id, "exit", obj("_tag", "Success", "value", value)))
}

// fail answers a request with a typed error.
func (a *Adapter) fail(id int, err *O) error {
	return a.write(obj("_tag", "Exit", "requestId", id, "exit", obj(
		"_tag", "Failure",
		"cause", []any{obj("_tag", "Fail", "error", err)},
	)))
}

// chunk pushes values on a subscription.
func (a *Adapter) chunk(id int, values ...any) error {
	return a.write(obj("_tag", "Chunk", "requestId", id, "values", values))
}

// partBytes is how much of a frame one socket message carries, as the Dev
// server splits them: a client that opened with frameParts=1 joins them.
const partBytes = 48 * 1024

// part is the mark on every part of a split frame but the last. No JSON frame
// begins with it.
const part = "\x1e"

func (a *Adapter) write(f *O) error {
	b, err := f.MarshalJSON()
	if err != nil {
		return err
	}
	a.send.Lock()
	defer a.send.Unlock()
	if !a.parts {
		return a.cfg.Send(b)
	}
	for _, p := range split(string(b)) {
		if err := a.cfg.Send([]byte(p)); err != nil {
			return err
		}
	}
	return nil
}

// split cuts a frame into parts of at most partBytes UTF-8 bytes, never inside
// a character, each but the last led by the part mark.
func split(s string) []string {
	if len(s) <= partBytes {
		return []string{s}
	}
	var out []string
	start, n := 0, len(part)
	for i := 0; i < len(s); {
		_, size := utf8.DecodeRuneInString(s[i:])
		if n+size > partBytes {
			out = append(out, part+s[start:i])
			start, n = i, len(part)
		}
		n += size
		i += size
	}
	return append(out, s[start:])
}

// uuid is a random version 4 UUID.
func uuid() string {
	var b [16]byte
	_, _ = rand.Read(b[:])
	b[6], b[8] = b[6]&0x0f|0x40, b[8]&0x3f|0x80
	return fmt.Sprintf("%x-%x-%x-%x-%x", b[0:4], b[4:6], b[6:8], b[8:10], b[10:])
}

func ts(t time.Time) string { return t.UTC().Format("2006-01-02T15:04:05.000Z") }

func (a *Adapter) now() string { return ts(a.cfg.Now()) }
