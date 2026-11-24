package frame

// ImportMethod is the Dev server's request that takes chat threads in as run-less
// history (packages/contracts chatThreads.import).
const ImportMethod = "chatThreads.import"

// ImportRequest is the Request frame that carries every thread this Adapter holds
// into a Dev server: what a session sends the real server in a cell when it
// upgrades into one, so the client's threads are there, under the ids it holds,
// with their conversations. id is the frame's request id, which the server's Exit
// names. A thread's messages are its user and assistant messages as they stand; a
// reply still streaming is carried as far as it got. With no threads it is nil.
func (a *Adapter) ImportRequest(id int) ([]byte, error) {
	a.mu.Lock()
	threads := make([]any, 0, len(a.st.Threads))
	for _, th := range a.st.Threads {
		msgs := []any{}
		for _, m := range th.Messages {
			role := m.Str("role")
			if (role != "user" && role != "assistant") || m.Str("text") == "" {
				continue
			}
			msgs = append(msgs, obj("role", role, "text", m.Str("text"), "createdAt", m.Str("createdAt")))
		}
		t := th.Thread
		threads = append(threads, obj(
			"threadId", th.ID,
			"title", t.Str("title"),
			"modelSelection", t.Get("modelSelection"),
			"createdAt", t.Str("createdAt"),
			"updatedAt", t.Str("updatedAt"),
			"messages", msgs,
		))
	}
	a.mu.Unlock()
	if len(threads) == 0 {
		return nil, nil
	}
	return obj("_tag", "Request", "id", id, "tag", ImportMethod,
		"payload", obj("threads", threads), "headers", []any{}).MarshalJSON()
}
