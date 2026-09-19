// Package dev runs the dev reasoning core (HIP-1330) inside the calling
// process.
//
// The core is this repository's loop — crates/core behind the C ABI of
// crates/ffi — built as a wasm module, which this package embeds and runs
// under wazero: no cgo, and no Rust toolchain in a build that imports it. A
// Core compiles the module once. A Session is one instance of it, with memory
// of its own, holding one conversation: step it with an Event, perform the
// Actions it answers with, and step it again with what they observed.
//
// The module imports environ_get, environ_sizes_get, fd_write and proc_exit,
// and nothing else. It reaches no file, socket, clock or source of randomness,
// so every effect an Action names is the host's to perform or refuse.
package dev

import (
	"bytes"
	"context"
	_ "embed"
	json "encoding/json/v2"
	"errors"
	"fmt"
	"math"
	"sync/atomic"

	"github.com/hanzoai/wasm"
	"github.com/tetratelabs/wazero/sys"
)

// ABI is the version of the C ABI, and of the payload encoding behind it,
// that this package speaks. Open refuses a module that answers dev_abi with
// any other.
const ABI = 1

// The status codes of crates/ffi/include/dev.h. Match them with errors.Is.
var (
	ErrHandle    = errors.New("dev: the handle names no live session")
	ErrMalformed = errors.New("dev: the payload is not a message of this protocol")
	ErrPoison    = errors.New("dev: a previous call on this session panicked")
	ErrPanic     = errors.New("dev: this call panicked; the session is now poisoned")
	ErrNull      = errors.New("dev: a pointer the caller must supply was null")
	ErrBusy      = errors.New("dev: the session is inside another call")
	ErrFull      = errors.New("dev: no slot is left for another session")
)

// statuses is dev.h's table, indexed by the negated code.
var statuses = [...]error{nil, ErrHandle, ErrMalformed, ErrPoison, ErrPanic, ErrNull, ErrBusy, ErrFull}

// module is target/wasm32-wasip1/release/dev.wasm, as `make wasm` copies it.
//
//go:embed dev.wasm
var module []byte

const (
	// pages bounds one session's memory: 1024 pages of 64 KiB, 64 MiB.
	pages = 1024
	// buf is the size of a dev_buf on wasm32: ptr, len and cap, three
	// little-endian u32s.
	buf = 12
)

// Core is the compiled module. Open one per process, and New or Restore a
// Session from it per conversation. A Core is safe for concurrent use.
type Core struct {
	engine *wasm.Engine
	module *wasm.Module
}

// Open compiles the embedded module, the one slow call, and checks that it
// speaks ABI.
func Open(ctx context.Context) (*Core, error) {
	return open(ctx, module)
}

func open(ctx context.Context, src []byte) (*Core, error) {
	engine, err := wasm.New(ctx, wasm.Limits{Pages: pages})
	if err != nil {
		return nil, err
	}
	c := &Core{engine: engine}
	if c.module, err = engine.Compile(ctx, src); err == nil {
		err = c.speaks(ctx)
	}
	if err != nil {
		_ = engine.Close(ctx)
		return nil, err
	}
	return c, nil
}

// speaks refuses a module that answers dev_abi with another ABI than ours.
func (c *Core) speaks(ctx context.Context) error {
	inst, err := c.module.Start(ctx)
	if err != nil {
		return err
	}
	defer inst.Close(ctx)
	out, err := inst.Call(ctx, "dev_abi")
	if err != nil {
		return err
	}
	if abi := uint32(out[0]); abi != ABI {
		return fmt.Errorf("dev: the module speaks ABI %d, and this package ABI %d", abi, ABI)
	}
	return nil
}

// Close releases the runtime. A Session started from it has nothing left to
// run in, and answers ErrHandle from then on.
func (c *Core) Close(ctx context.Context) error {
	return c.engine.Close(ctx)
}

// New starts a session.
func (c *Core) New(ctx context.Context, cfg Config) (*Session, error) {
	in, err := json.Marshal(cfg)
	if err != nil {
		return nil, fmt.Errorf("%w: %v", ErrMalformed, err)
	}
	return c.start(ctx, "dev_new", in)
}

// Restore rebuilds a session from its Snapshot, in an instance of its own.
//
// A snapshot damaged in storage — truncated, corrupted, or written under
// another ABI — answers ErrMalformed. The checksum is not a signature: a host
// that stores snapshots where others can write must authenticate them itself.
// Intact bytes are not proof that the sequence agrees with the host's journal
// either: a snapshot older than the journal mints again ids the host has
// already dispatched under, so reconcile the two before dispatching.
func (c *Core) Restore(ctx context.Context, state []byte) (*Session, error) {
	return c.start(ctx, "dev_restore", state)
}

func (c *Core) start(ctx context.Context, name string, in []byte) (*Session, error) {
	inst, err := c.module.Start(ctx)
	if err != nil {
		return nil, err
	}
	s := &Session{inst: inst}
	if err := s.open(ctx, name, in); err != nil {
		_ = s.Close(ctx)
		return nil, err
	}
	return s, nil
}

// Session is one conversation, in a wasm instance of its own: the unit of
// isolation. What a session does to its memory, a panic included, reaches no
// other.
//
// A Session is not safe for concurrent use. It runs one call at a time and
// does not queue: a call that arrives while another is running answers
// ErrBusy, as DEV_BUSY does in C, and never reaches the instance.
//
// A call runs under its ctx, and each call into the instance under the 5s
// hanzoai/wasm allows one. A call either one ends is stopped where it stands,
// and the session with it: it answers ErrHandle, which matches
// context.Canceled or context.DeadlineExceeded too, and every call after
// answers ErrHandle. Restore the last Snapshot to carry on.
type Session struct {
	inst   *wasm.Instance
	handle uint64
	// out is the session's one out-parameter, lent at birth and gone with the
	// instance: the handle is written there, then every dev_buf after it.
	out  uint32
	busy atomic.Bool
	// gone is what every call answers once the instance can answer no more:
	// ErrHandle once it is closed, ErrPoison once it has trapped.
	gone error
}

// Step feeds the core one event and returns the actions it answers with, in
// the order it asks for them. An event it cannot read answers ErrMalformed
// and changes nothing.
func (s *Session) Step(ctx context.Context, event Event) ([]Action, error) {
	in, err := json.Marshal(event)
	if err != nil {
		return nil, fmt.Errorf("%w: %v", ErrMalformed, err)
	}
	var actions []Action
	err = s.step(ctx, in, func(out []byte) error {
		if err := json.Unmarshal(out, &actions, json.RejectUnknownMembers(true)); err != nil {
			return fmt.Errorf("%w: the core's answer: %v", ErrMalformed, err)
		}
		return nil
	})
	return actions, err
}

// step is Step on encoded bytes: use reads the encoded answer before it goes
// back to the core.
func (s *Session) step(ctx context.Context, in []byte, use func([]byte) error) error {
	if err := s.enter(); err != nil {
		return err
	}
	defer s.leave()
	return s.lend(ctx, in, func(ptr uint32) error {
		if err := status(s.call(ctx, "dev_step", s.handle, uint64(ptr), uint64(len(in)), uint64(s.out))); err != nil {
			return err
		}
		return s.take(ctx, use)
	})
}

// Snapshot is the core's own state, one of the three facts a session is
// (HIP-1330): packed under the ABI with a checksum, for Restore.
func (s *Session) Snapshot(ctx context.Context) ([]byte, error) {
	if err := s.enter(); err != nil {
		return nil, err
	}
	defer s.leave()
	if err := status(s.call(ctx, "dev_snapshot", s.handle, uint64(s.out))); err != nil {
		return nil, err
	}
	var state []byte
	err := s.take(ctx, func(out []byte) error {
		state = bytes.Clone(out)
		return nil
	})
	return state, err
}

// Close closes the session's instance, and the session and every byte it held
// go with it. A call after Close answers ErrHandle, and a second Close answers
// nil.
func (s *Session) Close(ctx context.Context) error {
	if !s.busy.CompareAndSwap(false, true) {
		return ErrBusy
	}
	defer s.leave()
	if s.inst == nil {
		return nil
	}
	err := s.inst.Close(ctx)
	s.inst, s.gone = nil, ErrHandle
	return err
}

// open lends the out-parameter and runs dev_new or dev_restore into it.
func (s *Session) open(ctx context.Context, name string, in []byte) error {
	out, err := s.alloc(ctx, buf)
	if err != nil {
		return err
	}
	s.out = out
	err = s.lend(ctx, in, func(ptr uint32) error {
		return status(s.call(ctx, name, uint64(ptr), uint64(len(in)), uint64(s.out)))
	})
	if err != nil {
		return err
	}
	s.handle, _ = s.inst.Memory().ReadUint64Le(s.out)
	return nil
}

// enter holds the session for one call. What a gone session answers is read
// before it lets go: after, a Close on another goroutine may be writing it.
func (s *Session) enter() error {
	if !s.busy.CompareAndSwap(false, true) {
		return ErrBusy
	}
	if gone := s.gone; gone != nil {
		s.leave()
		return gone
	}
	return nil
}

func (s *Session) leave() {
	s.busy.Store(false)
}

// call runs one export and answers with its first result.
func (s *Session) call(ctx context.Context, name string, args ...uint64) (uint64, error) {
	if s.gone != nil {
		return 0, s.gone
	}
	out, err := s.inst.Call(ctx, name, args...)
	if err != nil {
		return 0, s.lose(err)
	}
	if len(out) == 0 {
		return 0, nil
	}
	return out[0], nil
}

// lose answers for a call the instance could not finish, and marks what every
// later call answers. An instance that has closed holds no session: ErrHandle,
// wrapping why it closed, so a call its context ended matches that context's
// error too. Anything else is a trap — a panic, or memory the core needed and
// was refused — after which the instance's memory is not trusted again:
// ErrPanic now, ErrPoison after.
func (s *Session) lose(err error) error {
	if _, ok := errors.AsType[*sys.ExitError](err); ok {
		s.gone = ErrHandle
		return fmt.Errorf("%w: %w", ErrHandle, err)
	}
	s.gone = ErrPoison
	return fmt.Errorf("%w: %v", ErrPanic, err)
}

// status turns what an export answered into dev.h's error for it.
func status(code uint64, err error) error {
	if err != nil {
		return err
	}
	n := -int64(int32(code))
	if n < 0 || n >= int64(len(statuses)) {
		return fmt.Errorf("dev: status %d is not one ABI %d has", int32(code), ABI)
	}
	return statuses[n]
}

// alloc borrows n bytes inside the instance. dev_alloc's length is an i32,
// and the core lends no more than math.MaxInt32 bytes, so a longer n is
// refused here rather than cut to its low 32 bits.
func (s *Session) alloc(ctx context.Context, n int) (uint32, error) {
	if n > math.MaxInt32 {
		return 0, fmt.Errorf("%w: %d bytes cannot be lent", ErrNull, n)
	}
	ptr, err := s.call(ctx, "dev_alloc", uint64(n))
	if err != nil {
		return 0, err
	}
	if ptr == 0 {
		return 0, fmt.Errorf("%w: the core lent nothing for %d bytes", ErrNull, n)
	}
	return uint32(ptr), nil
}

// lend copies in into memory borrowed from the instance for as long as use
// runs, and gives it back afterwards whatever use answered. Nothing is lent
// for nothing: an empty input crosses as a null pointer and a zero length.
func (s *Session) lend(ctx context.Context, in []byte, use func(ptr uint32) error) error {
	if len(in) == 0 {
		return use(0)
	}
	ptr, err := s.alloc(ctx, len(in))
	if err != nil {
		return err
	}
	if s.inst.Memory().Write(ptr, in) {
		err = use(ptr)
	} else {
		err = s.lose(fmt.Errorf("the core lent %d bytes at %#x, outside its memory", len(in), ptr))
	}
	if _, released := s.call(ctx, "dev_release", uint64(ptr), uint64(len(in))); err == nil {
		err = released
	}
	return err
}

// take hands use the bytes of the dev_buf the last call wrote to out, then
// frees the buffer whatever use answered. dev_free takes a dev_buf by value,
// which the wasm32 C ABI passes as a pointer to a copy: out is one.
func (s *Session) take(ctx context.Context, use func([]byte) error) error {
	mem := s.inst.Memory()
	ptr, _ := mem.ReadUint32Le(s.out)
	n, _ := mem.ReadUint32Le(s.out + 4)
	var err error
	if answer, ok := mem.Read(ptr, n); ok {
		err = use(answer)
	} else {
		err = s.lose(fmt.Errorf("the core answered %d bytes at %#x, outside its memory", n, ptr))
	}
	if _, freed := s.call(ctx, "dev_free", uint64(s.out)); err == nil {
		err = freed
	}
	return err
}
