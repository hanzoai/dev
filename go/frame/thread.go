package frame

import "strings"

// envelope is the frame every domain event travels in.
func (a *Adapter) envelope(th *thread, typ, run, node, driver string, payload *O) *O {
	e := obj("id", "event:thread:"+th.ID+":"+a.cfg.NewID(), "threadId", th.ID)
	if run != "" {
		e.Set("runId", run)
	}
	if node != "" {
		e.Set("nodeId", node)
	}
	if driver != "" {
		e.Set("driver", driver)
	}
	return e.Set("providerInstanceId", "hanzo").Set("occurredAt", a.now()).Set("type", typ).Set("payload", payload)
}

// apply folds an event's payload into the thread, as the Dev server's
// projection does.
func (th *thread) apply(typ string, p *O) {
	switch {
	case typ == "thread.visit-recorded":
		// The payload is the part of the thread a visit changes.
		for _, k := range p.Keys() {
			th.Thread.Set(k, p.Get(k))
		}
	case strings.HasPrefix(typ, "thread."):
		th.Thread = p.Clone()
	case strings.HasPrefix(typ, "run-attempt."):
		up(&th.Attempts, p.Clone())
	case strings.HasPrefix(typ, "run."):
		up(&th.Runs, p.Clone())
	case typ == "node.updated":
		up(&th.Nodes, p.Clone())
	case typ == "provider-thread.updated":
		up(&th.PThreads, p.Clone())
	case typ == "provider-turn.updated":
		up(&th.PTurns, p.Clone())
	case typ == "message.updated":
		up(&th.Messages, p.Clone())
	case typ == "turn-item.updated":
		up(&th.Items, p.Clone())
	}
}

// emit commits events to a thread: each takes the next sequence, goes to the
// thread's subscribers, and the shell hears the thread changed once, at the
// last. The caller holds a.mu.
func (a *Adapter) emit(th *thread, evs ...*O) error {
	for _, e := range evs {
		a.st.Seq++
		th.apply(e.Str("type"), e.Obj("payload"))
		item := obj("kind", "event", "sequence", a.st.Seq, "event", e)
		for id, s := range a.subs {
			if s.kind == subThread && s.thread == th.ID {
				if err := a.chunk(id, item); err != nil {
					return err
				}
			}
		}
	}
	return a.shellUpdate(th)
}

// shellUpdate recomputes a thread's summary and tells the shell.
func (a *Adapter) shellUpdate(th *thread) error {
	th.summarize()
	item := obj("kind", "thread.updated", "sequence", a.st.Seq, "location", "active", "thread", th.Shell.Clone())
	for id, s := range a.subs {
		if s.kind == subShell {
			if err := a.chunk(id, item); err != nil {
				return err
			}
		}
	}
	return nil
}

// summarize derives the shell's view of a thread from its projection.
func (th *thread) summarize() {
	t, s := th.Thread, th.Shell
	for _, k := range []string{
		"createdBy", "creationSource", "id", "projectId", "title", "providerInstanceId", "modelSelection",
		"runtimeMode", "interactionMode", "branch", "worktreePath", "lineage", "forkedFrom",
		"activeProviderThreadId", "createdAt", "updatedAt", "archivedAt", "settledOverride", "settledAt",
		"snoozedUntil", "snoozedAt", "lastVisitedAt", "titleRegeneration", "deletedAt",
	} {
		if t.Has(k) {
			s.Set(k, t.Get(k))
		}
	}
	if t.Has("opens") {
		s.Set("opens", t.Get("opens"))
	}
	s.Set("lastError", nilIfEmpty(th.Error))
	s.Set("itemCount", len(th.Items)).Set("visibleItemCount", len(th.Items))
	run := (*O)(nil)
	if n := len(th.Runs); n > 0 {
		run = th.Runs[n-1]
	}
	if run == nil {
		s.Set("status", "idle").Set("activityRunStatus", nil).Set("activeRunId", nil).Set("latestRunId", nil)
		return
	}
	st := run.Str("status")
	active := st != "completed" && st != "failed" && st != "cancelled" && st != "interrupted"
	s.Set("latestRunId", run.Get("id")).
		Set("latestRunRequestedAt", run.Get("requestedAt")).
		Set("latestRunStartedAt", run.Get("startedAt")).
		Set("latestRunCompletedAt", run.Get("completedAt"))
	s.Set("status", st)
	if active {
		s.Set("activityRunStatus", st).Set("activeRunId", run.Get("id")).Set("activityRunStartedAt", orNil(run.Get("startedAt"), run.Get("requestedAt")))
		if st == "waiting" {
			s.Set("activeRunId", nil)
		}
	} else {
		s.Set("activityRunStatus", nil).Set("activeRunId", nil).Set("activityRunStartedAt", nil)
	}
	for i := len(th.Messages) - 1; i >= 0; i-- {
		if m := th.Messages[i]; m.Str("role") == "user" {
			s.Set("latestUserMessageAt", m.Get("createdAt")).Set("latestUserAuthoredMessageAt", m.Get("createdAt"))
			break
		}
	}
}

func orNil(vs ...any) any {
	for _, v := range vs {
		if v != nil {
			return v
		}
	}
	return nil
}

func nilIfEmpty(s string) any {
	if s == "" {
		return nil
	}
	return s
}

// projection is the whole thread as a client reads it.
func (th *thread) projection(now string) *O {
	p := load("projection")
	p.Set("thread", th.Thread.Clone())
	for k, l := range map[string][]*O{
		"runs": th.Runs, "attempts": th.Attempts, "nodes": th.Nodes, "providerThreads": th.PThreads,
		"providerTurns": th.PTurns, "messages": th.Messages, "turnItems": th.Items,
	} {
		c := make([]*O, len(l))
		for i := range l {
			c[i] = l[i].Clone()
		}
		p.Set(k, c)
	}
	vis := make([]*O, len(th.Items))
	for i, it := range th.Items {
		vis[i] = obj("position", i, "visibility", "local", "sourceThreadId", th.ID, "sourceItemId", it.Str("id"), "item", it.Clone())
	}
	// A key the template has keeps its place: the template is the order the
	// server writes.
	p.Set("visibleTurnItems", vis)
	p.Set("updatedAt", th.Thread.Get("updatedAt"))
	_ = now
	return p
}

// newThread makes the thread a launch asks for. It holds no run yet.
func (a *Adapter) newThread(id, project string, in *O) *thread {
	now := a.now()
	t := load("thread")
	t.Set("createdBy", "user").Set("creationSource", firstNonEmpty(in.Str("creationSource"), "web"))
	t.Set("id", id).Set("projectId", project).Set("title", in.Str("title"))
	t.Set("providerInstanceId", "hanzo").Set("modelSelection", in.Get("modelSelection"))
	t.Set("runtimeMode", in.Str("runtimeMode")).Set("interactionMode", in.Str("interactionMode"))
	t.Set("branch", nil).Set("worktreePath", scratch+"/"+now[:10]+"-"+slug(in.Str("title"))+"-"+id[len(id)-8:])
	t.Set("activeProviderThreadId", pthreadID(id))
	t.Set("lineage", obj("parentThreadId", nil, "relationshipToParent", nil, "rootThreadId", id))
	t.Set("createdAt", now).Set("updatedAt", now).Set("lastVisitedAt", now)
	t.Set("archivedAt", nil).Set("settledOverride", nil).Set("settledAt", nil).Set("deletedAt", nil)
	t.Set("snoozedUntil", nil).Set("snoozedAt", nil)
	t.Set("titleRegeneration", nil)
	th := &thread{ID: id, Thread: t, Shell: load("shell").Obj("thread")}
	th.Shell.Set("id", id)
	return th
}

func firstNonEmpty(ss ...string) string {
	for _, s := range ss {
		if s != "" {
			return s
		}
	}
	return ""
}

// slug is a title as the Dev server names a scratch directory.
func slug(s string) string {
	var b strings.Builder
	words := 0
	for _, w := range strings.FieldsFunc(strings.ToLower(s), func(r rune) bool { return !(r >= 'a' && r <= 'z' || r >= '0' && r <= '9') }) {
		if words == 5 {
			break
		}
		if b.Len() > 0 {
			b.WriteByte('-')
		}
		b.WriteString(w)
		words++
	}
	if b.Len() == 0 {
		return "chat"
	}
	return b.String()
}

// The entities of a run. Each is a fresh copy of a recorded one with the
// run's own ids and the status the stage asks for.

func (a *Adapter) runEntity(th *thread, n int, status string, requested string, started, completed any) *O {
	rid := runID(th.ID, n)
	r := load("ev_run_updated").Obj("event").Obj("payload")
	if old := find(th.Runs, rid); old != nil {
		r = old.Clone()
	}
	r.Set("id", rid).Set("threadId", th.ID).Set("ordinal", n).Set("providerInstanceId", "hanzo")
	r.Set("modelSelection", th.Thread.Get("modelSelection")).Set("providerThreadId", pthreadID(th.ID))
	r.Set("rootNodeId", rootID(rid)).Set("activeAttemptId", attemptID(rid)).Set("status", status)
	r.Set("queuePosition", nil).Set("requestedAt", requested).Set("startedAt", started).Set("completedAt", completed)
	r.Set("checkpointId", nil).Set("contextHandoffId", nil)
	if !r.Has("workspacePreparation") {
		r.Set("workspacePreparation", obj("type", "existing_worktree", "worktreePath", th.Thread.Get("worktreePath")))
	}
	return r
}

func (a *Adapter) attemptEntity(th *thread, n int, status string, started, completed any) *O {
	rid := runID(th.ID, n)
	t := load("ev_run-attempt_updated").Obj("event").Obj("payload")
	t.Set("id", attemptID(rid)).Set("runId", rid).Set("attemptOrdinal", 1).Set("rootNodeId", rootID(rid))
	t.Set("providerInstanceId", "hanzo").Set("providerThreadId", pthreadID(th.ID)).Set("providerTurnId", nil)
	t.Del("nativeThreadId")
	if status != "pending" {
		t.Set("nativeThreadId", th.ID)
	}
	t.Set("reason", "initial").Set("status", status).Set("startedAt", started).Set("completedAt", completed)
	return t
}

func (a *Adapter) rootEntity(th *thread, n int, status string, started, completed any) *O {
	rid := runID(th.ID, n)
	t := load("ev_node").Obj("event").Obj("payload")
	t.Set("id", rootID(rid)).Set("threadId", th.ID).Set("runId", rid).Set("parentNodeId", nil).Set("rootNodeId", rootID(rid))
	t.Set("kind", "root_turn").Set("status", status).Set("countsForRun", true).Set("providerThreadId", pthreadID(th.ID))
	t.Set("providerTurnId", nil).Set("nativeItemRef", nil).Set("runtimeRequestId", nil).Set("checkpointScopeId", nil)
	t.Set("startedAt", started).Set("completedAt", completed)
	return t
}

func (a *Adapter) pthreadEntity(th *thread, status string, first, last int, created string) *O {
	t := load("ev_provider-thread_updated").Obj("event").Obj("payload")
	t.Set("id", pthreadID(th.ID)).Set("driver", "hanzo").Set("providerInstanceId", "hanzo").Set("providerSessionId", nil)
	t.Set("appThreadId", th.ID).Set("ownerNodeId", nil)
	t.Set("nativeThreadRef", nil)
	if status != "not_loaded" {
		t.Set("nativeThreadRef", obj("driver", "hanzo", "nativeId", th.ID, "strength", "strong"))
	}
	t.Set("status", status).Set("firstRunOrdinal", first).Set("lastRunOrdinal", last)
	t.Set("createdAt", created).Set("updatedAt", a.now())
	return t
}

func (a *Adapter) pturnEntity(th *thread, n int, status string, started, completed any) *O {
	rid := runID(th.ID, n)
	t := load("ev_provider-turn_updated").Obj("event").Obj("payload")
	t.Set("id", pturnID(rid)).Set("providerThreadId", pthreadID(th.ID)).Set("nodeId", rootID(rid)).Set("runAttemptId", attemptID(rid))
	t.Set("nativeTurnRef", obj("driver", "hanzo", "nativeId", esc(rid), "strength", "strong"))
	t.Set("ordinal", n).Set("status", status).Set("startedAt", started).Set("completedAt", completed)
	return t
}

func (a *Adapter) userMessage(th *thread, n int, id, text string, atts any, creation string) *O {
	rid, now := runID(th.ID, n), a.now()
	m := load("usermsg")
	m.Set("createdBy", "user").Set("creationSource", firstNonEmpty(creation, "web")).Set("id", id).Set("threadId", th.ID)
	m.Set("runId", rid).Set("nodeId", rootID(rid)).Set("role", "user").Set("text", text).Set("attachments", atts)
	m.Set("streaming", false).Set("createdAt", now).Set("updatedAt", now)
	return m
}

func (a *Adapter) userItem(th *thread, n int, msg *O) *O {
	rid := runID(th.ID, n)
	it := load("useritem")
	it.Set("id", userItemID(msg.Str("id"))).Set("threadId", th.ID).Set("runId", rid).Set("nodeId", rootID(rid))
	it.Set("providerThreadId", pthreadID(th.ID)).Set("providerTurnId", nil).Set("nativeItemRef", nil).Set("parentItemId", nil)
	it.Set("ordinal", 1000000+len(th.Items)+1).Set("status", "completed").Set("title", nil)
	it.Set("startedAt", msg.Get("createdAt")).Set("completedAt", msg.Get("createdAt")).Set("updatedAt", msg.Get("createdAt"))
	it.Set("createdBy", "user").Set("creationSource", msg.Get("creationSource")).Set("type", "user_message")
	it.Set("messageId", msg.Str("id")).Set("inputIntent", "turn_start").Set("text", msg.Get("text")).Set("attachments", msg.Get("attachments"))
	return it
}

// reply is the assistant's k-th message of run n, and the node and item that
// carry it.
func (a *Adapter) reply(th *thread, n, k int, text string, streaming bool, started string) (node, msg, item *O) {
	rid, now := runID(th.ID, n), a.now()
	done := any(now)
	if streaming {
		done = nil
	}
	node = load("ev_replynode").Obj("event").Obj("payload")
	node.Set("id", replyNodeID(rid, k)).Set("threadId", th.ID).Set("runId", rid).Set("parentNodeId", rootID(rid)).Set("rootNodeId", rootID(rid))
	node.Set("kind", "assistant_message").Set("status", map[bool]string{true: "running", false: "completed"}[streaming])
	node.Set("providerThreadId", pthreadID(th.ID)).Set("providerTurnId", pturnID(rid))
	node.Set("nativeItemRef", obj("driver", "hanzo", "nativeId", replyNative(rid, k), "strength", "strong"))
	node.Set("startedAt", started).Set("completedAt", done)
	msg = load("ev_replymsg").Obj("event").Obj("payload")
	msg.Set("createdBy", "agent").Set("creationSource", "provider").Set("id", replyMsgID(rid, k)).Set("threadId", th.ID)
	msg.Set("runId", rid).Set("nodeId", replyNodeID(rid, k)).Set("role", "assistant").Set("text", text).Set("attachments", []any{})
	msg.Set("streaming", streaming).Set("createdAt", started).Set("updatedAt", now)
	item = load("ev_replyitem").Obj("event").Obj("payload")
	ord := 1000000 + len(th.Items) + 1
	if old := find(th.Items, replyItemID(rid, k)); old != nil {
		if o, ok := old.Int("ordinal"); ok {
			ord = o
		}
	}
	item.Set("id", replyItemID(rid, k)).Set("threadId", th.ID).Set("runId", rid).Set("nodeId", replyNodeID(rid, k))
	item.Set("providerThreadId", pthreadID(th.ID)).Set("providerTurnId", pturnID(rid))
	item.Set("nativeItemRef", obj("driver", "hanzo", "nativeId", replyNative(rid, k), "strength", "strong")).Set("parentItemId", nil)
	item.Set("ordinal", ord).Set("status", map[bool]string{true: "running", false: "completed"}[streaming]).Set("title", nil)
	item.Set("startedAt", started).Set("completedAt", done).Set("updatedAt", now)
	item.Set("type", "assistant_message").Set("messageId", replyMsgID(rid, k)).Set("text", text).Set("streaming", streaming)
	return node, msg, item
}
