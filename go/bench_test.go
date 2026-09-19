package dev

import (
	"context"
	"testing"
)

// Open is the compile, paid once per process.
func BenchmarkOpen(b *testing.B) {
	ctx := context.Background()
	for b.Loop() {
		c, err := Open(ctx)
		if err != nil {
			b.Fatal(err)
		}
		_ = c.Close(ctx)
	}
}

// New is an instance and a session in it, and Close to give both back.
func BenchmarkNew(b *testing.B) {
	ctx := context.Background()
	c := core(b)
	for b.Loop() {
		s, err := c.New(ctx, Config{})
		if err != nil {
			b.Fatal(err)
		}
		_ = s.Close(ctx)
	}
}

// Step is one step of a session with a turn in flight: a turn that queues
// behind it, then the cancel that takes it out again. Both answer with
// actions, and neither leaves the session larger than it found it.
func BenchmarkStep(b *testing.B) {
	s := session(b, core(b), Config{})
	step(b, s, turn(1, "fix the build"))
	id := uint64(1)
	for b.Loop() {
		if id++; id%2 == 0 {
			step(b, s, turn(id, "and the tests"))
		} else {
			step(b, s, Event{Cancel: &Cancel{Turn: id - 1}})
		}
	}
}

// Snapshot is the state of a session one turn old.
func BenchmarkSnapshot(b *testing.B) {
	ctx := context.Background()
	s := finished(b, core(b))
	for b.Loop() {
		if _, err := s.Snapshot(ctx); err != nil {
			b.Fatal(err)
		}
	}
}

// Restore is an instance and that snapshot unpacked into it, and Close to
// give it back.
func BenchmarkRestore(b *testing.B) {
	ctx := context.Background()
	c := core(b)
	state, err := finished(b, c).Snapshot(ctx)
	if err != nil {
		b.Fatal(err)
	}
	for b.Loop() {
		s, err := c.Restore(ctx, state)
		if err != nil {
			b.Fatal(err)
		}
		_ = s.Close(ctx)
	}
}

// finished is a session that has run one turn: a read, and an answer.
func finished(b *testing.B, c *Core) *Session {
	s := session(b, c, Config{Model: new("zen5")})
	step(b, s, turn(1, "fix the build"))
	step(b, s, Event{Model: &Answer{ID: 1, Reply: Reply{Calls: []Call{{Read: &Read{Path: "src/lib.rs"}}}}}})
	step(b, s, Event{File: &Output{ID: 2, Text: "pub fn f() -> u8 { 1 }"}})
	step(b, s, Event{Model: &Answer{ID: 3, Reply: Reply{Text: new("It returns one.")}}})
	return s
}
