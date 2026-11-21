package frame

// empty answers the read side of a workspace the way the Dev server answers it
// for a thread that has none: not a repository, no files, no terminals, no
// previews. A client asks these of every thread as it opens it, and an error
// would only make it ask again. Methods that act on a workspace are not here;
// they need a sandbox.
func (a *Adapter) empty(id int, tag string, p *O) (error, bool) {
	exit := func(v any) (error, bool) { return a.exit(id, v), true }
	stream := func(vals ...any) (error, bool) {
		a.mu.Lock()
		defer a.mu.Unlock()
		a.subs[id] = &sub{kind: subStatic}
		if len(vals) == 0 {
			return nil, true
		}
		return a.chunk(id, vals...), true
	}
	switch tag {
	case "vcs.listRefs":
		return exit(obj("refs", []any{}, "isRepo", false, "hasPrimaryRemote", false, "nextCursor", nil, "totalCount", 0))
	case "projects.readFile":
		if allow, _ := p.Get("allowMissing").(bool); allow {
			return exit(obj("relativePath", p.Str("relativePath"), "contents", "", "byteLength", 0, "truncated", false, "exists", false))
		}
	case "preview.list":
		return exit(obj("sessions", []any{}, "serverEpoch", uuidFrom("epoch:"+a.st.Env.ID), "revision", 0))
	case "preview.reportProfiles":
		return exit(nil)
	case "subscribeVcsStatus":
		return stream(obj("_tag", "snapshot", "local", obj(
			"isRepo", false, "hasPrimaryRemote", false, "isDefaultRef", false, "refName", nil, "hasWorkingTreeChanges", false,
			"workingTree", obj("files", []any{}, "insertions", 0, "deletions", 0)), "remote", nil))
	case "subscribeVcsBranch":
		return stream(obj("refName", nil, "isRepo", false))
	case "subscribeTerminalMetadata":
		return stream(obj("type", "snapshot", "terminals", []any{}, "managerId", uuidFrom("terminals:"+a.st.Env.ID)))
	case "subscribeWorktreeSetup":
		return stream(nil)
	case "subscribeDiscoveredLocalServers":
		return stream(obj("servers", []any{}, "scannedAt", a.now(), "configuredUrlProbing", true))
	case "scheduledTasks.subscribe":
		return stream(obj("tasks", []any{}, "timeZone", "UTC"))
	case "subscribeProjectClones":
		return stream([]any{})
	case "subscribePreviewEvents":
		return stream()
	}
	return nil, false
}
