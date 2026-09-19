package dev

import (
	"bytes"
	"context"
	json "encoding/json/v2"
	"errors"
	"fmt"
	"math"
	"strings"
	"sync"
	"testing"
	"time"
)

// shared is one Core for every test that does not close its own. Open is the
// once-per-process call, and under -race a compile takes seconds.
var shared = sync.OnceValues(func() (*Core, error) {
	return Open(context.Background())
})

func core(t testing.TB) *Core {
	t.Helper()
	c, err := shared()
	if err != nil {
		t.Fatal(err)
	}
	return c
}

func session(t testing.TB, c *Core, cfg Config) *Session {
	t.Helper()
	s, err := c.New(t.Context(), cfg)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = s.Close(context.Background()) })
	return s
}

func step(t testing.TB, s *Session, event Event) []Action {
	t.Helper()
	actions, err := s.Step(t.Context(), event)
	if err != nil {
		t.Fatalf("step %s: %v", show(event), err)
	}
	return actions
}

func turn(id uint64, prompt string) Event {
	return Event{Turn: &Turn{ID: id, Prompt: prompt}}
}

// show is v as the protocol encodes it.
func show(v any) string {
	out, err := json.Marshal(v)
	if err != nil {
		return fmt.Sprintf("%#v (%v)", v, err)
	}
	return string(out)
}

// same fails the test unless got and want are the same message.
func same(t testing.TB, what string, got, want any) {
	t.Helper()
	if g, w := show(got), show(want); g != w {
		t.Errorf("%s\n got %s\nwant %s", what, g, w)
	}
}

// The whole loop once: a prompt, a model that asks for two tools, their
// results, and a model that answers in words.
func TestATurnRunsToDone(t *testing.T) {
	s := session(t, core(t), Config{})

	same(t, "a turn", step(t, s, turn(1, "fix the build")), []Action{
		{ID: 1, Op: Op{Model: &Ask{Messages: []Message{{User: new("fix the build")}}}}},
	})

	build := Exec{Argv: []string{"cargo", "build"}}
	lib := Read{Path: "src/lib.rs"}
	two := step(t, s, Event{Model: &Answer{ID: 1, Reply: Reply{
		Text:  new("Let me look."),
		Calls: []Call{{Exec: &build}, {Read: &lib}},
	}}})
	same(t, "two calls", two, []Action{
		{ID: 2, Op: Op{Exec: &build}},
		{ID: 3, Op: Op{Read: &lib}},
	})

	failed := "error[E0425]: cannot find value `x` in this scope"
	if actions := step(t, s, Event{Exec: &Output{ID: 2, Text: failed, Failed: true}}); len(actions) != 0 {
		t.Fatalf("one result of two asked for %s", show(actions))
	}
	same(t, "both results", step(t, s, Event{File: &Output{ID: 3, Text: "pub fn f() -> u8 { x }"}}), []Action{
		{ID: 4, Op: Op{Model: &Ask{Messages: []Message{
			{User: new("fix the build")},
			{Agent: &Agent{Text: new("Let me look."), Calls: []Dispatch{
				{ID: 2, Call: Call{Exec: &build}},
				{ID: 3, Call: Call{Read: &lib}},
			}}},
			{Tool: &Tool{ID: 2, Text: failed, Failed: true}},
			{Tool: &Tool{ID: 3, Text: "pub fn f() -> u8 { x }"}},
		}}}},
	})

	same(t, "a text answer", step(t, s, Event{Model: &Answer{ID: 4, Reply: Reply{Text: new("`x` was never declared.")}}}), []Action{
		{ID: 5, Op: Op{Emit: new("`x` was never declared.")}},
		{ID: 6, Op: Op{Save: true}},
		{ID: 7, Op: Op{Done: &Done{Turn: 1, Outcome: Outcome{Complete: true}}}},
	})
}

// A turn the core has accepted is refused at its id and below, so a prompt
// delivered twice runs once.
func TestARedeliveredTurnAsksForNothing(t *testing.T) {
	s := session(t, core(t), Config{})
	step(t, s, turn(7, "fix the build"))
	for _, id := range []uint64{7, 6, 1} {
		if actions := step(t, s, turn(id, "fix the build")); len(actions) != 0 {
			t.Errorf("turn %d again asked for %s", id, show(actions))
		}
	}
	step(t, s, Event{Model: &Answer{ID: 1, Reply: Reply{Calls: []Call{{Read: &Read{Path: "Makefile"}}}}}})
	same(t, "the transcript", step(t, s, Event{File: &Output{ID: 2, Text: "all: build"}}), []Action{
		{ID: 3, Op: Op{Model: &Ask{Messages: []Message{
			{User: new("fix the build")},
			{Agent: &Agent{Calls: []Dispatch{{ID: 2, Call: Call{Read: &Read{Path: "Makefile"}}}}}},
			{Tool: &Tool{ID: 2, Text: "all: build"}},
		}}}},
	})
}

// A result is taken only while its id is outstanding and was dispatched to
// that family. Anything else changes nothing, and in particular does not use
// up the entry the genuine result still needs.
func TestAResultNoOneAwaitsChangesNothing(t *testing.T) {
	s := session(t, core(t), Config{})
	step(t, s, turn(1, "fix the build"))
	build := Exec{Argv: []string{"make"}}
	file := Read{Path: "Makefile"}
	step(t, s, Event{Model: &Answer{ID: 1, Reply: Reply{Calls: []Call{{Exec: &build}, {Read: &file}}}}})

	// 2 went out as an Exec and 3 as a File; 1 is answered; 99 never went out.
	for _, event := range []Event{
		{File: &Output{ID: 2, Text: "forged"}},
		{Git: &Output{ID: 2, Text: "forged"}},
		{Exec: &Output{ID: 3, Text: "forged"}},
		{Browse: &Output{ID: 3, Text: "forged"}},
		{Exec: &Output{ID: 99, Text: "forged"}},
		{Model: &Answer{ID: 1, Reply: Reply{Text: new("forged")}}},
		{Model: &Answer{ID: 2, Reply: Reply{Text: new("forged")}}},
	} {
		if actions := step(t, s, event); len(actions) != 0 {
			t.Errorf("%s asked for %s", show(event), show(actions))
		}
	}

	if actions := step(t, s, Event{Exec: &Output{ID: 2, Text: "made"}}); len(actions) != 0 {
		t.Fatalf("one result of two asked for %s", show(actions))
	}
	same(t, "the genuine results", step(t, s, Event{File: &Output{ID: 3, Text: "all: build"}}), []Action{
		{ID: 4, Op: Op{Model: &Ask{Messages: []Message{
			{User: new("fix the build")},
			{Agent: &Agent{Calls: []Dispatch{{ID: 2, Call: Call{Exec: &build}}, {ID: 3, Call: Call{Read: &file}}}}},
			{Tool: &Tool{ID: 2, Text: "made"}},
			{Tool: &Tool{ID: 3, Text: "all: build"}},
		}}}},
	})
	if actions := step(t, s, Event{File: &Output{ID: 3, Text: "all: build"}}); len(actions) != 0 {
		t.Errorf("a result delivered twice asked for %s", show(actions))
	}
}

// A call whose path climbs out of the workspace never becomes an action. The
// model is told, in the transcript, under the id the call was given.
func TestAPathOutOfTheWorkspaceIsRefused(t *testing.T) {
	s := session(t, core(t), Config{})
	step(t, s, turn(1, "read the password file"))
	passwd := Read{Path: "../../etc/passwd"}
	lib := Read{Path: "src/lib.rs"}
	same(t, "the call inside", step(t, s, Event{Model: &Answer{ID: 1, Reply: Reply{
		Calls: []Call{{Read: &passwd}, {Read: &lib}},
	}}}), []Action{{ID: 3, Op: Op{Read: &lib}}})

	told := []Message{
		{User: new("read the password file")},
		{Agent: &Agent{Calls: []Dispatch{{ID: 2, Call: Call{Read: &passwd}}, {ID: 3, Call: Call{Read: &lib}}}}},
		{Tool: &Tool{ID: 2, Text: "refused: ../../etc/passwd is not inside the workspace", Failed: true}},
		{Tool: &Tool{ID: 3, Text: "fn main() {}"}},
	}
	same(t, "the refusal", step(t, s, Event{File: &Output{ID: 3, Text: "fn main() {}"}}), []Action{
		{ID: 4, Op: Op{Model: &Ask{Messages: told}}},
	})

	// When every call is refused nothing will come back to resume the turn,
	// so the core asks the model again at once.
	hosts := Write{Path: "/etc/hosts", Text: "127.0.0.1 hanzo.ai"}
	told = append(told,
		Message{Agent: &Agent{Calls: []Dispatch{{ID: 5, Call: Call{Write: &hosts}}}}},
		Message{Tool: &Tool{ID: 5, Text: "refused: /etc/hosts is not inside the workspace", Failed: true}},
	)
	same(t, "every call refused", step(t, s, Event{Model: &Answer{ID: 4, Reply: Reply{Calls: []Call{{Write: &hosts}}}}}), []Action{
		{ID: 6, Op: Op{Model: &Ask{Messages: told}}},
	})
}

// A snapshot is enough to carry a session to another process: restored into
// a Core that never saw it, it continues the id sequence where it stopped.
func TestASnapshotRestoresIntoANewCore(t *testing.T) {
	ctx := t.Context()
	first, err := Open(ctx)
	if err != nil {
		t.Fatal(err)
	}
	s, err := first.New(ctx, Config{Model: new("zen5")})
	if err != nil {
		t.Fatal(err)
	}
	test := Exec{Argv: []string{"cargo", "test"}}
	step(t, s, turn(1, "run the tests"))
	same(t, "a call", step(t, s, Event{Model: &Answer{ID: 1, Reply: Reply{Calls: []Call{{Exec: &test}}}}}), []Action{
		{ID: 2, Op: Op{Exec: &test}},
	})
	state, err := s.Snapshot(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if err := s.Close(ctx); err != nil {
		t.Fatal(err)
	}
	if err := first.Close(ctx); err != nil {
		t.Fatal(err)
	}

	restored, err := core(t).Restore(ctx, state)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = restored.Close(context.Background()) })
	same(t, "the result", step(t, restored, Event{Exec: &Output{ID: 2, Text: "test result: ok"}}), []Action{
		{ID: 3, Op: Op{Model: &Ask{Model: new("zen5"), Messages: []Message{
			{User: new("run the tests")},
			{Agent: &Agent{Calls: []Dispatch{{ID: 2, Call: Call{Exec: &test}}}}},
			{Tool: &Tool{ID: 2, Text: "test result: ok"}},
		}}}},
	})
	same(t, "the end", step(t, restored, Event{Model: &Answer{ID: 3, Reply: Reply{Text: new("green")}}}), []Action{
		{ID: 4, Op: Op{Emit: new("green")}},
		{ID: 5, Op: Op{Save: true}},
		{ID: 6, Op: Op{Done: &Done{Turn: 1, Outcome: Outcome{Complete: true}}}},
	})
}

// No truncation of a snapshot and no single flipped bit restores: the ABI
// stamp, the checksum and the body each refuse it.
func TestADamagedSnapshotIsMalformed(t *testing.T) {
	ctx := t.Context()
	c := core(t)
	s := session(t, c, Config{})
	step(t, s, turn(1, "fix the build"))
	state, err := s.Snapshot(ctx)
	if err != nil {
		t.Fatal(err)
	}

	refused := func(what string, damaged []byte) {
		t.Helper()
		restored, err := c.Restore(ctx, damaged)
		if !errors.Is(err, ErrMalformed) {
			t.Errorf("%s restored with %v", what, err)
		}
		if restored != nil {
			_ = restored.Close(ctx)
		}
	}
	for cut := range len(state) {
		refused(fmt.Sprintf("the first %d of %d bytes", cut, len(state)), state[:cut])
	}
	for i := range state {
		for bit := range 8 {
			flipped := bytes.Clone(state)
			flipped[i] ^= 1 << bit
			refused(fmt.Sprintf("bit %d of byte %d flipped", bit, i), flipped)
		}
	}

	restored, err := c.Restore(ctx, state)
	if err != nil {
		t.Fatalf("the intact snapshot: %v", err)
	}
	defer restored.Close(ctx)
	same(t, "the intact snapshot", step(t, restored, Event{Model: &Answer{ID: 1, Reply: Reply{Text: new("done")}}}), []Action{
		{ID: 2, Op: Op{Emit: new("done")}},
		{ID: 3, Op: Op{Save: true}},
		{ID: 4, Op: Op{Done: &Done{Turn: 1, Outcome: Outcome{Complete: true}}}},
	})
}

func TestAClosedSessionAnswersErrHandle(t *testing.T) {
	ctx := t.Context()
	s, err := core(t).New(ctx, Config{})
	if err != nil {
		t.Fatal(err)
	}
	step(t, s, turn(1, "fix the build"))
	if err := s.Close(ctx); err != nil {
		t.Fatal(err)
	}
	if _, err := s.Step(ctx, turn(2, "fix the build")); !errors.Is(err, ErrHandle) {
		t.Errorf("a step after Close answered %v", err)
	}
	if _, err := s.Snapshot(ctx); !errors.Is(err, ErrHandle) {
		t.Errorf("a snapshot after Close answered %v", err)
	}
	if err := s.Close(ctx); err != nil {
		t.Errorf("a second Close answered %v", err)
	}
}

// Closing a Core ends the sessions started from it: a call answers ErrHandle,
// and nothing crashes on the way.
func TestAClosedCoreEndsItsSessions(t *testing.T) {
	ctx := t.Context()
	c, err := Open(ctx)
	if err != nil {
		t.Fatal(err)
	}
	s, err := c.New(ctx, Config{})
	if err != nil {
		t.Fatal(err)
	}
	step(t, s, turn(1, "fix the build"))
	state, err := s.Snapshot(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if err := c.Close(ctx); err != nil {
		t.Fatal(err)
	}
	for range 2 {
		if _, err := s.Step(ctx, turn(2, "fix the build")); !errors.Is(err, ErrHandle) {
			t.Errorf("a step after the Core closed answered %v", err)
		}
		if _, err := s.Snapshot(ctx); !errors.Is(err, ErrHandle) {
			t.Errorf("a snapshot after the Core closed answered %v", err)
		}
	}
	if err := s.Close(ctx); err != nil {
		t.Errorf("Close after the Core closed answered %v", err)
	}
	if _, err := c.New(ctx, Config{}); err == nil {
		t.Error("a closed Core started a session")
	}
	if _, err := c.Restore(ctx, state); err == nil {
		t.Error("a closed Core restored a session")
	}
}

// A call its context ends is stopped where it stands, and the session with
// it: ErrHandle, matching the context's own error too, and ErrHandle for every
// call after. The snapshot taken before carries the session on.
func TestAStepItsContextEndsEndsTheSession(t *testing.T) {
	ctx := t.Context()
	c := core(t)
	cancelled, cancel := context.WithCancel(ctx)
	cancel()
	past, stop := context.WithDeadline(ctx, time.Now().Add(-time.Hour))
	defer stop()
	for _, ended := range []struct {
		ctx context.Context
		err error
	}{{cancelled, context.Canceled}, {past, context.DeadlineExceeded}} {
		s := session(t, c, Config{})
		step(t, s, turn(1, "fix the build"))
		state, err := s.Snapshot(ctx)
		if err != nil {
			t.Fatal(err)
		}
		answer := Event{Model: &Answer{ID: 1, Reply: Reply{Text: new("done")}}}
		if actions, err := s.Step(ended.ctx, answer); !errors.Is(err, ErrHandle) || !errors.Is(err, ended.err) {
			t.Errorf("a step under %v answered %s, %v", ended.err, show(actions), err)
		}
		if _, err := s.Step(ctx, answer); !errors.Is(err, ErrHandle) {
			t.Errorf("the step after %v answered %v", ended.err, err)
		}
		if _, err := s.Snapshot(ctx); !errors.Is(err, ErrHandle) {
			t.Errorf("a snapshot after %v answered %v", ended.err, err)
		}
		if fresh, err := c.New(ended.ctx, Config{}); !errors.Is(err, ErrHandle) || !errors.Is(err, ended.err) {
			t.Errorf("New under %v answered %v", ended.err, err)
			if fresh != nil {
				_ = fresh.Close(ctx)
			}
		}

		restored, err := c.Restore(ctx, state)
		if err != nil {
			t.Fatal(err)
		}
		same(t, "the restored session", step(t, restored, answer), []Action{
			{ID: 2, Op: Op{Emit: new("done")}},
			{ID: 3, Op: Op{Save: true}},
			{ID: 4, Op: Op{Done: &Done{Turn: 1, Outcome: Outcome{Complete: true}}}},
		})
		_ = restored.Close(ctx)
	}
}

// Open is bounded the same way: a module whose dev_abi never returns is
// stopped when the context ends, rather than holding its goroutine, and every
// garbage collection after, forever.
func TestOpenStopsAModuleThatNeverAnswers(t *testing.T) {
	// (module (func (export "dev_abi") (result i32) (loop (br 0)) (i32.const 1)))
	spin := []byte{
		0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
		0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f,
		0x03, 0x02, 0x01, 0x00,
		0x07, 0x0b, 0x01, 0x07, 'd', 'e', 'v', '_', 'a', 'b', 'i', 0x00, 0x00,
		0x0a, 0x0b, 0x01, 0x09, 0x00, 0x03, 0x40, 0x0c, 0x00, 0x0b, 0x41, 0x01, 0x0b,
	}
	ctx, cancel := context.WithTimeout(t.Context(), 100*time.Millisecond)
	defer cancel()
	done := make(chan error, 1)
	go func() {
		c, err := open(ctx, spin)
		if err == nil {
			_ = c.Close(context.Background())
		}
		done <- err
	}()
	select {
	case err := <-done:
		if !errors.Is(err, context.DeadlineExceeded) {
			t.Errorf("open answered %v", err)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("open is still running 10s after its deadline")
	}
}

// A Session runs one call at a time and refuses, rather than queues, the
// next: the instance never sees two.
func TestASessionAnswersOneCallAtATime(t *testing.T) {
	ctx := t.Context()
	s := session(t, core(t), Config{})
	s.busy.Store(true)
	if _, err := s.Step(ctx, turn(1, "fix the build")); !errors.Is(err, ErrBusy) {
		t.Errorf("a step during another call answered %v", err)
	}
	if _, err := s.Snapshot(ctx); !errors.Is(err, ErrBusy) {
		t.Errorf("a snapshot during another call answered %v", err)
	}
	if err := s.Close(ctx); !errors.Is(err, ErrBusy) {
		t.Errorf("Close during another call answered %v", err)
	}
	s.busy.Store(false)
	same(t, "the step after", step(t, s, turn(1, "fix the build")), []Action{
		{ID: 1, Op: Op{Model: &Ask{Messages: []Message{{User: new("fix the build")}}}}},
	})
}

// One goroutine steps a session whose instance has gone while another closes
// it, and the session's busy flag is all that orders them. Under -race this
// holds a call to reading what it answers while it still holds the flag: read
// after it lets go, the answer races the Close that writes it.
func TestAGoneSessionSteppedAndClosedAtOnce(t *testing.T) {
	ctx := t.Context()
	c, err := Open(ctx)
	if err != nil {
		t.Fatal(err)
	}
	var sessions []*Session
	for range 16 {
		s, err := c.New(ctx, Config{})
		if err != nil {
			t.Fatal(err)
		}
		sessions = append(sessions, s)
	}
	if err := c.Close(ctx); err != nil {
		t.Fatal(err)
	}
	for _, s := range sessions {
		// The first call finds the instance closed with the Core, and marks the
		// session gone.
		if _, err := s.Step(ctx, turn(1, "fix the build")); !errors.Is(err, ErrHandle) {
			t.Fatalf("a step after the Core closed answered %v", err)
		}
		stepped := make(chan struct{})
		var wg sync.WaitGroup
		wg.Go(func() {
			for i := range 200 {
				_, err := s.Step(ctx, turn(1, "fix the build"))
				if !errors.Is(err, ErrHandle) && !errors.Is(err, ErrBusy) {
					t.Errorf("a step on a gone session answered %v", err)
				}
				if i == 0 {
					close(stepped)
				}
			}
		})
		<-stepped
		for {
			err := s.Close(ctx)
			if !errors.Is(err, ErrBusy) {
				if err != nil {
					t.Errorf("Close answered %v", err)
				}
				break
			}
		}
		wg.Wait()
	}
}

// Every session has an instance of its own, so thirty-two of them stepped at
// once each see their own conversation and their own ids, and nothing else.
func TestSessionsSteppedAtOnceStayApart(t *testing.T) {
	c := core(t)
	var wg sync.WaitGroup
	for i := range 32 {
		wg.Go(func() {
			if err := converse(t.Context(), c, i); err != nil {
				t.Errorf("session %d: %v", i, err)
			}
		})
	}
	wg.Wait()
}

// converse runs three turns on a session of its own, each reading a file
// named for it, and checks every answer against what that session alone
// should see.
func converse(ctx context.Context, c *Core, i int) error {
	model := fmt.Sprintf("model-%d", i)
	s, err := c.New(ctx, Config{Model: &model})
	if err != nil {
		return err
	}
	defer s.Close(context.Background())
	var transcript []Message
	next := uint64(1)
	expect := func(event Event, want ...Action) error {
		got, err := s.Step(ctx, event)
		if err != nil {
			return err
		}
		if g, w := show(got), show(want); g != w {
			return fmt.Errorf("%s\n got %s\nwant %s", show(event), g, w)
		}
		return nil
	}
	for n := range uint64(3) {
		prompt := fmt.Sprintf("session %d, turn %d", i, n)
		read := Read{Path: fmt.Sprintf("session-%d/turn-%d.go", i, n)}
		transcript = append(transcript, Message{User: &prompt})
		if err := expect(turn(n+1, prompt), Action{ID: next, Op: Op{Model: &Ask{Model: &model, Messages: transcript}}}); err != nil {
			return err
		}
		transcript = append(transcript, Message{Agent: &Agent{Calls: []Dispatch{{ID: next + 1, Call: Call{Read: &read}}}}})
		if err := expect(Event{Model: &Answer{ID: next, Reply: Reply{Calls: []Call{{Read: &read}}}}}, Action{ID: next + 1, Op: Op{Read: &read}}); err != nil {
			return err
		}
		transcript = append(transcript, Message{Tool: &Tool{ID: next + 1, Text: read.Path}})
		if err := expect(Event{File: &Output{ID: next + 1, Text: read.Path}}, Action{ID: next + 2, Op: Op{Model: &Ask{Model: &model, Messages: transcript}}}); err != nil {
			return err
		}
		transcript = append(transcript, Message{Agent: &Agent{Text: &prompt}})
		if err := expect(Event{Model: &Answer{ID: next + 2, Reply: Reply{Text: &prompt}}},
			Action{ID: next + 3, Op: Op{Emit: &prompt}},
			Action{ID: next + 4, Op: Op{Save: true}},
			Action{ID: next + 5, Op: Op{Done: &Done{Turn: n + 1, Outcome: Outcome{Complete: true}}}},
		); err != nil {
			return err
		}
		next += 6
	}
	return nil
}

// The host lends and frees in pairs: ten thousand steps, and a snapshot after
// each pair of them, leave the instance's memory exactly as large as it was.
func TestTenThousandStepsDoNotGrowMemory(t *testing.T) {
	ctx := t.Context()
	s := session(t, core(t), Config{})
	step(t, s, turn(1, strings.Repeat("fix the build ", 64)))

	// A turn that arrives mid-turn is queued, and a cancel takes it out of the
	// queue again: two steps that answer with actions and leave nothing behind
	// but a larger id.
	id := uint64(1)
	pair := func() {
		id++
		same(t, "a queued turn", step(t, s, turn(id, "and the tests")), []Action{{ID: 3*id - 4, Op: Op{Save: true}}})
		same(t, "its cancel", step(t, s, Event{Cancel: &Cancel{Turn: id}}), []Action{
			{ID: 3*id - 3, Op: Op{Save: true}},
			{ID: 3*id - 2, Op: Op{Done: &Done{Turn: id, Outcome: Outcome{Cancelled: true}}}},
		})
		if _, err := s.Snapshot(ctx); err != nil {
			t.Fatal(err)
		}
	}
	for range 100 {
		pair()
	}
	before := s.inst.Memory().Size()
	for range 5000 {
		pair()
	}
	if after := s.inst.Memory().Size(); after != before {
		t.Errorf("10,000 steps grew memory from %d to %d bytes", before, after)
	}
}

// A refused call gives back what it borrowed too. The core refusing an event
// it cannot read, and the host refusing an answer it was handed, each release
// the event's loan and free the answer, so a thousand of each leave the
// instance's memory as large as it was.
func TestRefusedCallsDoNotGrowMemory(t *testing.T) {
	ctx := t.Context()
	s := session(t, core(t), Config{})
	prompt := strings.Repeat("fix the build ", 1024)
	step(t, s, turn(1, prompt))

	unreadable := []byte(`{"Turn":{"id":2,"prompt":"` + prompt + `"}`)
	again, err := json.Marshal(turn(1, prompt))
	if err != nil {
		t.Fatal(err)
	}
	refused := errors.New("the host refuses the answer")
	refuse := func() {
		t.Helper()
		err := s.step(ctx, unreadable, func(out []byte) error {
			t.Errorf("an event the core cannot read was answered with %s", out)
			return nil
		})
		if !errors.Is(err, ErrMalformed) {
			t.Fatalf("an event the core cannot read answered %v", err)
		}
		// The core answers a redelivered turn with no actions, and the host
		// refuses even that.
		if err := s.step(ctx, again, func([]byte) error { return refused }); !errors.Is(err, refused) {
			t.Fatalf("an answer the host refused answered %v", err)
		}
	}
	for range 10 {
		refuse()
	}
	before := s.inst.Memory().Size()
	for range 1000 {
		refuse()
	}
	if after := s.inst.Memory().Size(); after != before {
		t.Errorf("2,000 refusals grew memory from %d to %d bytes", before, after)
	}
}

// A core that is refused memory it needs traps. The trap is its session's
// alone: that session answers ErrPanic, then ErrPoison until it is closed,
// and a session beside it carries on.
func TestATrapPoisonsOnlyItsSession(t *testing.T) {
	ctx := t.Context()
	c := core(t)
	s := session(t, c, Config{})
	beside := session(t, c, Config{})
	step(t, beside, turn(1, "fix the build"))

	// Copied in, decoded and cloned into an ask, 24 MiB of prompt needs more
	// than the 64 MiB a session is allowed.
	_, err := s.Step(ctx, turn(1, strings.Repeat("x", 24<<20)))
	if !errors.Is(err, ErrPanic) {
		t.Fatalf("a refused allocation answered %v", err)
	}
	if _, err := s.Step(ctx, turn(2, "fix the build")); !errors.Is(err, ErrPoison) {
		t.Errorf("a step after the trap answered %v", err)
	}
	if _, err := s.Snapshot(ctx); !errors.Is(err, ErrPoison) {
		t.Errorf("a snapshot after the trap answered %v", err)
	}
	if err := s.Close(ctx); err != nil {
		t.Errorf("Close after the trap answered %v", err)
	}

	same(t, "the session beside it", step(t, beside, Event{Model: &Answer{ID: 1, Reply: Reply{Text: new("fixed")}}}), []Action{
		{ID: 2, Op: Op{Emit: new("fixed")}},
		{ID: 3, Op: Op{Save: true}},
		{ID: 4, Op: Op{Done: &Done{Turn: 1, Outcome: Outcome{Complete: true}}}},
	})
}

// A length the core's i32 cannot carry is refused before the core is asked,
// rather than cut to its low 32 bits: 4 GiB and 16 bytes would be lent as 16,
// and writing the event into that would poison the session.
func TestALengthTheCoreCannotTakeIsNull(t *testing.T) {
	if math.MaxInt == math.MaxInt32 {
		t.Skip("no slice here is longer than the core can lend")
	}
	s := session(t, core(t), Config{})
	for _, n := range []uint64{math.MaxInt32 + 1, 1<<32 + 16} {
		if ptr, err := s.alloc(t.Context(), int(n)); !errors.Is(err, ErrNull) {
			t.Errorf("%d bytes were lent at %#x, %v", n, ptr, err)
		}
	}
	same(t, "the step after", step(t, s, turn(1, "fix the build")), []Action{
		{ID: 1, Op: Op{Model: &Ask{Messages: []Message{{User: new("fix the build")}}}}},
	})
}

// An event too large to lend is refused before the core sees it, and the
// session is left as it was.
func TestAnEventTooLargeToLendIsNull(t *testing.T) {
	ctx := t.Context()
	s := session(t, core(t), Config{})
	if _, err := s.Step(ctx, turn(1, strings.Repeat("x", 65<<20))); !errors.Is(err, ErrNull) {
		t.Fatalf("an event larger than the session's memory answered %v", err)
	}
	same(t, "the step after", step(t, s, turn(1, "fix the build")), []Action{
		{ID: 1, Op: Op{Model: &Ask{Messages: []Message{{User: new("fix the build")}}}}},
	})
}
