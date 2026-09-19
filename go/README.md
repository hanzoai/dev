# github.com/hanzoai/dev/go

The dev loop (HIP-1330), for a Go host.

The loop is the Rust core in this repository, built as a wasm module and
embedded here. This package runs it in the calling process under wazero: no
cgo, and no Rust toolchain in the build that imports it. The core reasons and
the host acts. The core asks for work as `Action`s, and the host performs them
and reports what happened as `Event`s.

```go
import dev "github.com/hanzoai/dev/go"
```

## The calls

    Open(ctx) (*Core, error)                          compile the module, once per process
    (*Core).New(ctx, Config) (*Session, error)        start a session in an instance of its own
    (*Core).Restore(ctx, state) (*Session, error)     rebuild one from a snapshot
    (*Session).Step(ctx, Event) ([]Action, error)     one event in, the actions it asks for out
    (*Session).Snapshot(ctx) ([]byte, error)          the core's state, for Restore
    (*Session).Close(ctx) error                       drop the session and its instance
    (*Core).Close(ctx) error                          release the runtime

A `Core` is safe for concurrent use. A `Session` is not: it runs one call at a
time, and a call made while another is running answers `ErrBusy`.

A call runs under its `ctx`, and each call into the instance under the 5s
hanzoai/wasm allows one. A call either one ends stops where it stands and ends
its session: it answers `ErrHandle`, which matches `context.Canceled` or
`context.DeadlineExceeded` too, and every call after it answers `ErrHandle`.
Restore the last snapshot to carry on.

The statuses of `crates/ffi/include/dev.h` are errors to match with
`errors.Is`: `ErrHandle`, `ErrMalformed`, `ErrPoison`, `ErrPanic`, `ErrNull`,
`ErrBusy` and `ErrFull`. A trap in the core, such as a panic or a refused
allocation, answers `ErrPanic` and then `ErrPoison` for that session only.
Other sessions keep running. A closed session answers `ErrHandle`.

## The types

The types are `crates/protocol`, field for field. A Rust enum is a struct whose
fields are its variants: a pointer for a variant that carries a value, a bool
for one that does not. Exactly one is set. A union that sets none, or two, is
refused as `ErrMalformed` before the core sees it.

    dev.Event{Turn: &dev.Turn{ID: 1, Prompt: "fix the build"}}
    dev.Event{Exec: &dev.Output{ID: a.ID, Text: out, Failed: code != 0}}
    dev.Event{Timer: true}

Reading one refuses what serde refuses of a union: a name it does not have, two
names, `null` for a variant's value, or no union at all where a struct holds
one. Any other member that is missing reads as its zero value, which serde
refuses unless the member is an `Option`.

Text crosses as UTF-8. A host that has read bytes that are not UTF-8 must turn
them into text before it steps with them.

## A loop

The model here is a stand-in: it asks for one file, and then answers.

```go
ctx := context.Background()
core, err := dev.Open(ctx)
if err != nil {
	log.Fatal(err)
}
defer core.Close(ctx)

s, err := core.New(ctx, dev.Config{Model: new("zen5")})
if err != nil {
	log.Fatal(err)
}
defer s.Close(ctx)

events := []dev.Event{{Turn: &dev.Turn{ID: 1, Prompt: "What does main.go do?"}}}
for len(events) > 0 {
	actions, err := s.Step(ctx, events[0])
	if err != nil {
		log.Fatal(err)
	}
	events = events[1:]
	for _, a := range actions {
		switch op := a.Op; {
		case op.Model != nil:
			events = append(events, dev.Event{Model: &dev.Answer{ID: a.ID, Reply: model(op.Model)}})
		case op.Read != nil:
			events = append(events, dev.Event{File: &dev.Output{ID: a.ID, Text: "package main\n"}})
		case op.Emit != nil:
			fmt.Println(*op.Emit)
		case op.Save:
			// Write s.Snapshot(ctx) beside the journal, at a.ID.
		case op.Done != nil:
			fmt.Println("done", op.Done.Turn)
		}
	}
}

func model(ask *dev.Ask) dev.Reply {
	if last := ask.Messages[len(ask.Messages)-1]; last.Tool == nil {
		return dev.Reply{Calls: []dev.Call{{Read: &dev.Read{Path: "main.go"}}}}
	}
	return dev.Reply{Text: new("It declares package main and does nothing.")}
}
```

`example_test.go` runs the same loop as a test.

## Persisting a session

A session is three facts, kept apart: the core's snapshot, the workspace, and
the host's journal of actions. Every action carries an id. When the host
records dispatch and completion under that id, a result can be replayed
instead of an effect run twice. The core refuses a turn id it has already
accepted and a result for an id it is not waiting on, so replaying an event
is safe.

A restored snapshot continues the id sequence where it stopped. Its checksum
catches damage in storage, and it is not a signature: a host that keeps
snapshots where others can write must authenticate them itself. Intact bytes do
not prove the snapshot agrees with the journal either. One older than the
journal mints ids the host has already dispatched, so reconcile the two before
dispatching again.

## The module

`dev.wasm` is `make wasm` at the repository root, copied here. Its imports are
`environ_get`, `environ_sizes_get`, `fd_write` and `proc_exit`, and nothing
else. The core cannot reach a file, a socket, a clock or a source of
randomness, and a test checks that list. Each session gets at most 64 MiB of
memory.
