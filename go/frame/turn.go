package frame

import (
	"context"
	"errors"
	"strings"

	dev "github.com/hanzoai/dev/go"
)

// prelude is what the loop is told a chat session is. There is no workspace,
// so there is nothing to read or run.
const prelude = "You are Hanzo Dev in chat mode. This conversation has no workspace: answer from what you know, and say so when a task needs files or a shell."

// turnStart is a run that has been recorded and not yet carried out.
type turnStart struct {
	th     *thread
	n      int
	prompt string
}

// begin records a message and the run it starts, both in the state the Dev
// server records them: a user message, its item, and a run that is starting.
// With events, the thread's subscribers hear each; without, the thread is new
// and the launch carries it whole. The caller holds a.mu.
func (a *Adapter) begin(th *thread, mid, text string, atts any, creation string, events bool) (*turnStart, error) {
	if mid == "" {
		mid = a.cfg.NewID()
	}
	if atts == nil {
		atts = []any{}
	}
	n := len(th.Runs) + 1
	now := a.now()
	first, created := n, now
	if len(th.PThreads) > 0 {
		first, _ = th.PThreads[0].Int("firstRunOrdinal")
		created = th.PThreads[0].Str("createdAt")
	}
	msg := a.userMessage(th, n, mid, text, atts, creation)
	run := a.runEntity(th, n, "starting", now, nil, nil)
	run.Set("userMessageId", mid)
	att := a.attemptEntity(th, n, "pending", nil, nil)
	root := a.rootEntity(th, n, "pending", nil, nil)
	pt := a.pthreadEntity(th, "not_loaded", first, n, created)
	rid, rootNode := run.Str("id"), root.Str("id")
	item := a.userItem(th, n, msg)
	th.Thread.Set("updatedAt", now)
	evs := []*O{
		a.envelope(th, "run.created", rid, rootNode, "", run),
		a.envelope(th, "run-attempt.created", rid, rootNode, "", att),
		a.envelope(th, "node.updated", rid, rootNode, "", root),
		a.envelope(th, "message.updated", rid, rootNode, "", msg),
		a.envelope(th, "turn-item.updated", rid, rootNode, "", item),
		a.envelope(th, "provider-thread.updated", "", "", "hanzo", pt),
	}
	if events {
		run.Del("workspacePreparation")
	}
	th.Error = ""
	if events {
		if err := a.emit(th, evs...); err != nil {
			return nil, err
		}
	} else {
		for _, e := range evs {
			a.st.Seq++
			th.apply(e.Str("type"), e.Obj("payload"))
		}
	}
	return &turnStart{th: th, n: n, prompt: text}, nil
}

// start carries a run out in the background. A thread runs one turn at a time:
// a message sent while one is running waits its turn, as the loop's ids rise.
func (a *Adapter) start(t *turnStart) {
	if t == nil {
		return
	}
	a.wg.Add(1)
	go func() {
		defer a.wg.Done()
		a.run(a.bg, t)
	}()
}

// commit sends a batch of events.
func (a *Adapter) commit(th *thread, evs ...*O) error {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.emit(th, evs...)
}

// session is the thread's loop, started from its snapshot when it has one.
func (a *Adapter) session(ctx context.Context, th *thread) (*dev.Session, error) {
	if th.sess != nil {
		return th.sess, nil
	}
	var err error
	if len(th.Core) > 0 {
		th.sess, err = a.cfg.Core.Restore(ctx, th.Core)
		return th.sess, err
	}
	model := th.Thread.Obj("modelSelection").Str("model")
	cfg := dev.Config{Prelude: new(prelude)}
	if model != "" {
		cfg.Model = &model
	}
	th.sess, err = a.cfg.Core.New(ctx, cfg)
	return th.sess, err
}

// reply is the assistant message a run is writing.
type reply struct {
	k       int
	text    string
	started string
}

// run carries out one turn: it hands the prompt to the loop and performs what
// the loop asks for until it says the turn is done, writing each step as the
// events the Dev server writes for the same.
func (a *Adapter) run(ctx context.Context, t *turnStart) {
	th, n := t.th, t.n
	th.turn.Lock()
	defer th.turn.Unlock()
	rid := runID(th.ID, n)
	rootNode := rootID(rid)
	var started string
	var cur *reply

	// fail ends the run, and says why.
	fail := func(err error) {
		a.mu.Lock()
		th.Error = err.Error()
		a.mu.Unlock()
		a.finish(th, n, started, "failed", "failed")
	}

	started = a.now()
	if err := a.commit(th,
		a.envelope(th, "run.updated", rid, rootNode, "", a.runEntity(th, n, "running", th.runRequested(n), started, nil)),
		a.envelope(th, "run-attempt.updated", rid, rootNode, "", a.attemptEntity(th, n, "running", started, nil)),
		a.envelope(th, "node.updated", rid, rootNode, "", a.rootEntity(th, n, "running", started, nil)),
		a.envelope(th, "provider-thread.updated", "", "", "hanzo", a.pthreadEntity(th, "active", th.firstRun(), n, th.created())),
		a.envelope(th, "provider-turn.updated", rid, rootNode, "hanzo", a.pturnEntity(th, n, "running", started, nil)),
	); err != nil {
		return
	}

	sess, err := a.session(ctx, th)
	if err != nil {
		fail(err)
		return
	}
	// show writes the assistant message the run is on, as it stands.
	show := func(text string, streaming bool) error {
		if cur == nil {
			a.mu.Lock()
			th.Replies++
			cur = &reply{k: th.Replies, started: a.now()}
			a.mu.Unlock()
		}
		cur.text = text
		a.mu.Lock()
		node, msg, item := a.reply(th, n, cur.k, text, streaming, cur.started)
		a.mu.Unlock()
		err := a.commit(th,
			a.envelope(th, "node.updated", rid, node.Str("id"), "hanzo", node),
			a.envelope(th, "message.updated", rid, node.Str("id"), "hanzo", msg),
			a.envelope(th, "turn-item.updated", rid, node.Str("id"), "hanzo", item),
		)
		if !streaming {
			cur = nil
		}
		return err
	}

	queue := []dev.Event{{Turn: &dev.Turn{ID: uint64(n), Prompt: t.prompt}}}
	outcome := "completed"
loop:
	for len(queue) > 0 {
		if ctx.Err() != nil {
			fail(ctx.Err())
			return
		}
		actions, err := sess.Step(ctx, queue[0])
		queue = queue[1:]
		if err != nil {
			fail(err)
			return
		}
		for _, act := range actions {
			op := act.Op
			switch {
			case op.Model != nil:
				text := ""
				r, err := a.cfg.Effects.Model(ctx, *op.Model, func(d string) {
					text += d
					_ = show(text, true)
				})
				if err != nil {
					fail(err)
					return
				}
				queue = append(queue, dev.Event{Model: &dev.Answer{ID: act.ID, Reply: r}})
			case op.Emit != nil:
				if err := show(*op.Emit, false); err != nil {
					return
				}
			case op.Save:
				if err := a.save(ctx, th, sess); err != nil {
					fail(err)
					return
				}
			case op.Done != nil:
				if op.Done.Outcome.Cancelled {
					outcome = "cancelled"
				}
				break loop
			default:
				out, err := a.cfg.Effects.Perform(ctx, op)
				if err != nil {
					var ns *NeedsSandbox
					if errors.As(err, &ns) || ctx.Err() != nil {
						fail(err)
						return
					}
					out = dev.Output{ID: act.ID, Text: err.Error(), Failed: true}
				}
				out.ID = act.ID
				queue = append(queue, outputEvent(op, out))
			}
		}
	}
	if cur != nil {
		_ = show(cur.text, false)
	}
	if err := a.save(ctx, th, sess); err != nil {
		fail(err)
		return
	}
	a.finish(th, n, started, outcome, outcome)
	a.retitle(ctx, th, t.prompt)
}

func (th *thread) firstRun() int {
	n, _ := th.PThreads[0].Int("firstRunOrdinal")
	return n
}

func (th *thread) created() string { return th.PThreads[0].Str("createdAt") }

func (th *thread) runRequested(n int) string {
	if r := find(th.Runs, runID(th.ID, n)); r != nil {
		return r.Str("requestedAt")
	}
	return ""
}

// outputEvent is what a finished Op is called when it is told to the loop.
func outputEvent(op dev.Op, out dev.Output) dev.Event {
	switch {
	case op.Exec != nil:
		return dev.Event{Exec: &out}
	case op.Git != nil:
		return dev.Event{Git: &out}
	case op.Browse != nil:
		return dev.Event{Browse: &out}
	}
	return dev.Event{File: &out}
}

// save keeps the loop's snapshot, and the Adapter's state with it.
func (a *Adapter) save(ctx context.Context, th *thread, sess *dev.Session) error {
	snap, err := sess.Snapshot(ctx)
	if err != nil {
		return err
	}
	a.mu.Lock()
	th.Core = snap
	state, err := a.st.marshal()
	a.mu.Unlock()
	if err != nil {
		return err
	}
	return a.cfg.Effects.Save(ctx, state)
}

// finish closes a run: its turn, attempt, node, provider thread and the run
// itself, in the order the Dev server closes them.
func (a *Adapter) finish(th *thread, n int, started, run, attempt string) {
	rid, rootNode, now := runID(th.ID, n), rootID(runID(th.ID, n)), a.now()
	a.mu.Lock()
	pt := th.PThreads[0]
	first, _ := pt.Int("firstRunOrdinal")
	created := pt.Str("createdAt")
	a.mu.Unlock()
	if started == "" {
		started = now
	}
	_ = a.commit(th,
		a.envelope(th, "provider-turn.updated", rid, rootNode, "hanzo", a.pturnEntity(th, n, run, started, now)),
		a.envelope(th, "run-attempt.updated", rid, rootNode, "", a.attemptEntity(th, n, attempt, started, now)),
		a.envelope(th, "provider-thread.updated", "", "", "hanzo", a.pthreadEntity(th, "idle", first, n, created)),
		a.envelope(th, "run.updated", rid, rootNode, "", a.runEntity(th, n, run, th.runRequested(n), started, now)),
		a.envelope(th, "node.updated", rid, rootNode, "", a.rootEntity(th, n, run, started, now)),
	)
}

// retitle names a thread whose title was asked for, from its first message,
// the way the Dev server does once a run is done: one ask of the model.
func (a *Adapter) retitle(ctx context.Context, th *thread, prompt string) {
	a.mu.Lock()
	want := th.Regenerate
	model := th.Thread.Obj("modelSelection").Str("model")
	a.mu.Unlock()
	if want == "" {
		return
	}
	ask := dev.Ask{Prelude: new("Reply with a title of at most five words for the conversation that begins with the message below. Reply with the title only."),
		Messages: []dev.Message{{User: &prompt}}}
	if model != "" {
		ask.Model = &model
	}
	r, err := a.cfg.Effects.Model(ctx, ask, func(string) {})
	title := ""
	if err == nil && r.Text != nil {
		title = strings.Trim(strings.TrimSpace(*r.Text), "\"'.")
	}
	a.mu.Lock()
	defer a.mu.Unlock()
	if th.Regenerate != want {
		return
	}
	th.Regenerate = ""
	t := th.Thread.Clone().Set("titleRegeneration", nil).Set("updatedAt", a.now())
	if title != "" {
		t.Set("title", title)
	}
	if !t.Has("titleRenamed") {
		t.Set("titleRenamed", false)
	}
	_ = a.emit(th, a.envelope(th, "thread.metadata-updated", "", "", "", t))
}
