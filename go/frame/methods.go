package frame

import (
	"context"
	"fmt"
)

func (a *Adapter) environment(e *O) {
	e.Set("environmentId", a.st.Env.ID)
	if a.st.Env.Label != "" {
		e.Set("label", a.st.Env.Label)
	}
}

// serverConfig answers subscribeServerConfig: the config, then the two
// settings that follow it.
func (a *Adapter) serverConfig(id int) error {
	cfg := load("config")
	c := cfg.Obj("config")
	a.environment(c.Obj("environment"))
	c.Set("serverTime", a.now())
	a.mu.Lock()
	defer a.mu.Unlock()
	a.subs[id] = &sub{kind: subConfig}
	return a.chunk(id, cfg, load("usagelimits"), load("themes"))
}

// lifecycle answers subscribeServerLifecycle with the welcome.
func (a *Adapter) lifecycle(id int) error {
	w := load("welcome")
	p := w.Obj("payload")
	a.environment(p.Obj("environment"))
	p.Set("cwd", scratch).Set("projectName", "chat")
	a.mu.Lock()
	defer a.mu.Unlock()
	a.subs[id] = &sub{kind: subLifecycle}
	return a.chunk(id, w)
}

// refreshProviders answers server.refreshProviders.
func (a *Adapter) refreshProviders(id int) error {
	return a.exit(id, load("refresh"))
}

// archived answers getArchivedShellSnapshot: nothing is archived here.
func (a *Adapter) archived(id int) error {
	r := load("archived")
	a.mu.Lock()
	defer a.mu.Unlock()
	r.Set("snapshotSequence", a.st.Seq).Set("projects", []*O{a.st.Project.Clone()}).Set("threads", []any{})
	return a.exit(id, r)
}

// subscribeShell answers the thread list: a snapshot, the end marker the
// client asked for, and from then on every change to a thread.
func (a *Adapter) subscribeShell(id int, p *O) error {
	a.mu.Lock()
	defer a.mu.Unlock()
	s := load("shellsnapshot")
	snap := s.Obj("snapshot")
	threads := make([]*O, 0, len(a.st.threads))
	for _, th := range a.st.Threads {
		threads = append(threads, th.Shell.Clone())
	}
	snap.Set("snapshotSequence", a.st.Seq).Set("threads", threads).Set("archivedThreads", []any{})
	snap.Set("projects", []*O{a.st.Project.Clone()})
	a.subs[id] = &sub{kind: subShell}
	vals := []any{s}
	if b, _ := p.Get("requestCompletionMarker").(bool); b {
		vals = append(vals, obj("kind", "synchronized"))
	}
	return a.chunk(id, vals...)
}

// subscribeThread answers one thread's transcript: a snapshot, the end
// marker, then each event as it commits.
func (a *Adapter) subscribeThread(id int, p *O) error {
	tid := p.Str("threadId")
	a.mu.Lock()
	defer a.mu.Unlock()
	th := a.st.threads[tid]
	if th == nil {
		return a.fail(id, obj(
			"_tag", "OrchestrationV2GetThreadProjectionError",
			"threadId", tid,
			"message", "Failed to load orchestration V2 thread "+tid,
		))
	}
	a.subs[id] = &sub{kind: subThread, thread: tid}
	vals := []any{obj("kind", "snapshot", "snapshotSequence", a.st.Seq, "projection", th.projection(a.now()))}
	if b, _ := p.Get("requestCompletionMarker").(bool); b {
		vals = append(vals, obj("kind", "synchronized"))
	}
	return a.chunk(id, vals...)
}

// commandError is the typed failure of a command.
func commandError(p *O, msg string) *O {
	return obj("_tag", "OrchestrationV2DispatchCommandError", "commandId", p.Str("commandId"), "commandType", p.Str("type"), "message", msg)
}

// launchThread creates a thread and sends its first message.
func (a *Adapter) launchThread(ctx context.Context, id int, p *O) error {
	tid := p.Str("threadId")
	if tid == "" {
		tid = a.cfg.NewID()
	}
	a.mu.Lock()
	if th := a.st.threads[tid]; th != nil {
		defer a.mu.Unlock()
		return a.exit(id, obj("threadId", tid, "projection", th.projection(a.now()), "resumed", true, "sequence", a.st.Seq))
	}
	if p.Str("projectId") != a.st.Project.Str("id") {
		a.mu.Unlock()
		return a.fail(id, obj("_tag", "OrchestrationV2ThreadLaunchError", "commandId", p.Str("commandId"), "projectId", p.Str("projectId"),
			"message", "This environment has one project, the chat scratch space."))
	}
	th := a.newThread(tid, p.Str("projectId"), p)
	a.st.threads[tid] = th
	a.st.Threads = append(a.st.Threads, th)
	a.st.Seq++
	if b, _ := p.Get("generateTitle").(bool); b {
		th.Regenerate = a.cfg.NewID()
		th.Thread.Set("titleRegeneration", obj("requestId", th.Regenerate, "startedAt", a.now()))
	}
	var turn *turnStart
	if m := p.Obj("initialMessage"); m != nil {
		var err error
		turn, err = a.begin(th, m.Str("messageId"), m.Str("text"), m.Get("attachments"), p.Str("creationSource"), false)
		if err != nil {
			a.mu.Unlock()
			return err
		}
	}
	th.summarize()
	err := a.shellUpdate(th)
	res := obj("threadId", tid, "projection", th.projection(a.now()), "resumed", false, "sequence", a.st.Seq)
	a.mu.Unlock()
	if err != nil {
		return err
	}
	if err := a.exit(id, res); err != nil {
		return err
	}
	a.start(turn)
	return nil
}

// dispatch answers the commands of the chat-mode subset.
func (a *Adapter) dispatch(ctx context.Context, id int, p *O) error {
	typ := p.Str("type")
	a.mu.Lock()
	th := a.st.threads[p.Str("threadId")]
	if th == nil {
		a.mu.Unlock()
		return a.fail(id, commandError(p, "There is no thread "+p.Str("threadId")+"."))
	}
	var turn *turnStart
	var err error
	now := a.now()
	switch typ {
	case "thread.visit":
		t := th.Thread.Clone().Set("lastVisitedAt", p.Get("visitedAt"))
		pl := obj("lastVisitedAt", p.Get("visitedAt"))
		if b, _ := p.Get("opened").(bool); b {
			opens := obj("openedAt", a.cfg.Now().UnixMilli(), "score", 1, "picks", []any{})
			t.Set("opens", opens)
			pl.Set("opens", opens)
		}
		th.Thread = t
		err = a.emit(th, a.envelope(th, "thread.visit-recorded", "", "", "", pl))
	case "thread.metadata.update":
		t := th.Thread.Clone()
		if title := p.Str("title"); title != "" {
			t.Set("title", title).Set("titleRenamed", true).Set("titleRegeneration", nil)
			th.Regenerate = ""
		}
		t.Set("updatedAt", now)
		err = a.emit(th, a.envelope(th, "thread.metadata-updated", "", "", "", t))
	case "thread.settle":
		t := th.Thread.Clone()
		t.Set("settledOverride", "settled").Set("settledAt", now).Set("unsettledAt", nil).Set("updatedAt", now)
		for _, k := range []string{"limitRecovery", "pinnedAt", "pinOrderKey", "activeOrderKey"} {
			if !t.Has(k) {
				t.Set(k, nil)
			}
		}
		if !t.Has("titleRenamed") {
			t.Set("titleRenamed", false)
		}
		err = a.emit(th, a.envelope(th, "thread.settled", "", "", "", t))
	case "message.dispatch":
		turn, err = a.begin(th, p.Str("messageId"), p.Str("text"), p.Get("attachments"), p.Str("creationSource"), true)
	default:
		a.mu.Unlock()
		return a.fail(id, commandError(p, fmt.Sprintf("%s is not part of the chat-mode subset.", typ)))
	}
	seq := a.st.Seq
	a.mu.Unlock()
	if err != nil {
		return err
	}
	if err := a.exit(id, obj("sequence", seq)); err != nil {
		return err
	}
	a.start(turn)
	return nil
}
