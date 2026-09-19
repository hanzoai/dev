package dev

import (
	"bytes"
	json "encoding/json/v2"
	"errors"
	"math"
	"testing"
)

// Every type in protocol.go crosses the real core and comes back as it went:
// serde reads what Go wrote, and Go reads what serde answered into the same
// values. Go also writes those values back byte for byte as the core wrote
// them, so a host that journals an action journals the core's own bytes.
func TestEveryTypeCrossesTheCoreIntact(t *testing.T) {
	ctx := t.Context()
	cfg := Config{
		Model:   new(`zen5 "quoted" <&> ` + " "),
		Prelude: new("tab\t newline\n nul\x00 bell\x07 del\x7f é 🦀 \\ /"),
	}
	s := session(t, core(t), cfg)
	exchange := func(event Event) []Action {
		t.Helper()
		in, err := json.Marshal(event)
		if err != nil {
			t.Fatal(err)
		}
		var answer []byte
		if err := s.step(ctx, in, func(out []byte) error {
			answer = bytes.Clone(out)
			return nil
		}); err != nil {
			t.Fatalf("%s: %v", in, err)
		}
		var actions []Action
		if err := json.Unmarshal(answer, &actions, json.RejectUnknownMembers(true)); err != nil {
			t.Fatalf("%s: %v", answer, err)
		}
		if again := show(actions); again != string(answer) {
			t.Errorf("Go writes the core's answer differently\ncore %s\n  go %s", answer, again)
		}
		return actions
	}

	prompt := "fix \"the\" build\n\t— naïve 😀 \x00   </script>"
	transcript := []Message{{User: &prompt}}
	same(t, "a turn", exchange(turn(1, prompt)), []Action{
		{ID: 1, Op: Op{Model: &Ask{Model: cfg.Model, Prelude: cfg.Prelude, Messages: transcript}}},
	})

	calls := []Call{
		{Read: &Read{Path: "src/ünï.rs"}},
		{Write: &Write{Path: "notes.md", Text: "# notes\n\n\"quoted\"\n"}},
		{Patch: &Patch{Path: "src/main.rs", Diff: "--- a\n+++ b\n@@ -1 +1 @@\n-old\n+new\n"}},
		{Exec: &Exec{Argv: []string{"cargo", "test", "--", "--nocapture"}, Cwd: new("crates/core")}},
		{Exec: &Exec{Argv: []string{"ls", "-la"}}},
		{Git: &Git{Argv: []string{"commit", "-m", "fix: the build"}}},
		{Browse: &Browse{URL: "https://hanzo.ai/?q=a&b=<c>", Body: new(`{"k":"v"}`)}},
		{Browse: &Browse{URL: "https://hanzo.ai/"}},
	}
	var dispatched []Action
	var dispatches []Dispatch
	for i, call := range calls {
		id := uint64(i) + 2
		op := Op{Read: call.Read, Write: call.Write, Patch: call.Patch, Exec: call.Exec, Git: call.Git, Browse: call.Browse}
		dispatched = append(dispatched, Action{ID: id, Op: op})
		dispatches = append(dispatches, Dispatch{ID: id, Call: call})
	}
	same(t, "every call", exchange(Event{Model: &Answer{ID: 1, Reply: Reply{Text: new(""), Calls: calls}}}), dispatched)
	transcript = append(transcript, Message{Agent: &Agent{Text: new(""), Calls: dispatches}})

	// Results arrive in any order, and the transcript keeps the order they
	// arrived in.
	for i := len(dispatched) - 1; i >= 0; i-- {
		a := dispatched[i]
		out := &Output{ID: a.ID, Text: "out " + show(a.Op), Failed: i%2 == 0}
		var event Event
		switch {
		case a.Op.Read != nil, a.Op.Write != nil, a.Op.Patch != nil:
			event.File = out
		case a.Op.Exec != nil:
			event.Exec = out
		case a.Op.Git != nil:
			event.Git = out
		case a.Op.Browse != nil:
			event.Browse = out
		}
		transcript = append(transcript, Message{Tool: &Tool{ID: out.ID, Text: out.Text, Failed: out.Failed}})
		want := []Action{}
		if i == 0 {
			want = []Action{{ID: 10, Op: Op{Model: &Ask{Model: cfg.Model, Prelude: cfg.Prelude, Messages: transcript}}}}
		}
		same(t, "a result", exchange(event), want)
	}

	// A timer asks for a save only when the sequence has moved since the last.
	same(t, "a timer", exchange(Event{Timer: true}), []Action{{ID: 11, Op: Op{Save: true}}})
	same(t, "a second timer", exchange(Event{Timer: true}), []Action{})

	// Text that is empty is still text: it is emitted, and the transcript
	// keeps it. No text at all is neither.
	same(t, "an empty answer", exchange(Event{Model: &Answer{ID: 10, Reply: Reply{Text: new("")}}}), []Action{
		{ID: 12, Op: Op{Emit: new("")}},
		{ID: 13, Op: Op{Save: true}},
		{ID: 14, Op: Op{Done: &Done{Turn: 1, Outcome: Outcome{Complete: true}}}},
	})
	transcript = append(transcript, Message{Agent: &Agent{Text: new("")}}, Message{User: new("again")})
	same(t, "a second turn", exchange(turn(2, "again")), []Action{
		{ID: 15, Op: Op{Model: &Ask{Model: cfg.Model, Prelude: cfg.Prelude, Messages: transcript}}},
	})
	same(t, "no answer at all", exchange(Event{Model: &Answer{ID: 15}}), []Action{
		{ID: 16, Op: Op{Save: true}},
		{ID: 17, Op: Op{Done: &Done{Turn: 2, Outcome: Outcome{Complete: true}}}},
	})

	// The largest id there is survives the trip, and a cancel names it.
	transcript = append(transcript, Message{User: new("last")})
	same(t, "the last turn", exchange(turn(math.MaxUint64, "last")), []Action{
		{ID: 18, Op: Op{Model: &Ask{Model: cfg.Model, Prelude: cfg.Prelude, Messages: transcript}}},
	})
	same(t, "a cancel", exchange(Event{Cancel: &Cancel{Turn: math.MaxUint64}}), []Action{
		{ID: 19, Op: Op{Save: true}},
		{ID: 20, Op: Op{Done: &Done{Turn: math.MaxUint64, Outcome: Outcome{Cancelled: true}}}},
	})
}

// A union that sets no variant, or two, is not a message of this protocol,
// and neither is text that is not UTF-8. Each is refused before the core is
// asked, and the session is left as it was.
func TestWhatIsNotAMessageIsRefusedBeforeTheCore(t *testing.T) {
	ctx := t.Context()
	c := core(t)
	s := session(t, c, Config{})
	for _, event := range []Event{
		{},
		{Timer: true, Cancel: &Cancel{Turn: 1}},
		{Turn: &Turn{ID: 1, Prompt: "fix the build"}, Model: &Answer{ID: 1}},
		{Model: &Answer{ID: 1, Reply: Reply{Calls: []Call{{}}}}},
		{Model: &Answer{ID: 1, Reply: Reply{Calls: []Call{{Read: &Read{Path: "a"}, Git: &Git{}}}}}},
		{Turn: &Turn{ID: 1, Prompt: "fix \xff the build"}},
	} {
		// The encoder is what refuses it: the core would too, and the test
		// would not know which of them had.
		if out, err := json.Marshal(event); err == nil {
			t.Errorf("%#v encoded as %s", event, out)
		}
		if _, err := s.Step(ctx, event); !errors.Is(err, ErrMalformed) {
			t.Errorf("%#v answered %v", event, err)
		}
	}
	if _, err := c.New(ctx, Config{Model: new("zen\xff")}); !errors.Is(err, ErrMalformed) {
		t.Errorf("a model name that is not UTF-8 answered %v", err)
	}
	same(t, "the first turn", step(t, s, turn(1, "fix the build")), []Action{
		{ID: 1, Op: Op{Model: &Ask{Messages: []Message{{User: new("fix the build")}}}}},
	})
}

// What the core cannot read it refuses with ErrMalformed, lends nothing back,
// and carries on.
func TestTheCoreRefusesWhatItCannotRead(t *testing.T) {
	ctx := t.Context()
	s := session(t, core(t), Config{})
	for _, payload := range []string{
		``,
		`not an event`,
		`"Turn"`,
		`{"Turn":{"id":1}}`,
		`{"Turn":{"id":-1,"prompt":"fix the build"}}`,
		`{"Turn":{"id":18446744073709551616,"prompt":"fix the build"}}`,
		`{"Bogus":{}}`,
		`{"Timer":null,"Cancel":{"turn":1}}`,
	} {
		err := s.step(ctx, []byte(payload), func(out []byte) error {
			t.Errorf("%s was answered with %s", payload, out)
			return nil
		})
		if !errors.Is(err, ErrMalformed) {
			t.Errorf("%q answered %v", payload, err)
		}
	}
	same(t, "the first turn", step(t, s, turn(1, "fix the build")), []Action{
		{ID: 1, Op: Op{Model: &Ask{Messages: []Message{{User: new("fix the build")}}}}},
	})
}

// Go reads a union as strictly as it writes one: a single variant, named as
// the protocol names it, carrying a value exactly when the variant does.
func TestAUnionIsReadAsOneVariant(t *testing.T) {
	for _, payload := range []string{
		`null`, `[]`, `{}`, `7`,
		`"Bogus"`, `{"Bogus":{}}`,
		`"save"`, `{"emit":"x"}`,
		`"Emit"`, `{"Save":null}`, `{"Save":true}`,
		`{"Emit":"x","Save":null}`,
		`{"Emit":"x","Done":{"turn":1,"outcome":"Complete"}}`,
		`{"Done":{"turn":1,"outcome":"Finished"}}`,
		`{"Done":{"turn":1,"outcome":"Complete","by":"me"}}`,
	} {
		var op Op
		if err := json.Unmarshal([]byte(payload), &op, json.RejectUnknownMembers(true)); err == nil {
			t.Errorf("%s was read as %s", payload, show(op))
		}
	}
	for payload, want := range map[string]Op{
		`"Save"`:       {Save: true},
		`{"Emit":""}`:  {Emit: new("")},
		`{"Emit":"x"}`: {Emit: new("x")},
		`{"Done":{"turn":1,"outcome":"Cancelled"}}`: {Done: &Done{Turn: 1, Outcome: Outcome{Cancelled: true}}},
	} {
		// A value read into carries nothing over from what it held before.
		op := Op{Model: &Ask{}}
		if err := json.Unmarshal([]byte(payload), &op); err != nil {
			t.Errorf("%s: %v", payload, err)
		}
		same(t, payload, op, want)
	}
}
