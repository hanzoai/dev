//! The C ABI the Go host drives the loop through.
//!
//! Nine symbols, an opaque `u64` per session, and a `Buf` for every byte
//! string that crosses back. The rules:
//!
//! - **Rust copies every input** before the call returns, so the host may free
//!   or reuse its buffer immediately.
//! - **Rust owns every output** until the host calls [`dev_free`]. A `Buf` is
//!   a Rust allocation; `free(3)` on it is wrong.
//! - **A host that shares no memory borrows some.** Compiled to wasm the
//!   library's memory is its own, so a pointer the host holds means nothing
//!   here: [`dev_alloc`] lends it a buffer inside, to write an input or receive
//!   an out-parameter, and [`dev_release`] takes it back. A native host never
//!   calls either — it already has memory both sides can see.
//! - **A handle is generational.** Dropping a session retires its handle, and
//!   a stale handle is refused with [`DEV_HANDLE`] rather than resolving to
//!   whatever took the slot next. A session dropped while one of its calls is
//!   running makes that call answer [`DEV_HANDLE`] too, and lend nothing out:
//!   there is no session left for the answer to belong to.
//! - **A panic does not cross the boundary.** Every entry point that has a
//!   status to answer with runs inside `catch_unwind`; an escaped panic answers
//!   [`DEV_PANIC`] and poisons that session, whose every later call answers
//!   [`DEV_POISON`] until it is dropped. The process stays up and the other
//!   sessions are untouched. [`dev_abi`] and [`dev_alloc`] are the exceptions
//!   and have no status channel to answer on: one reads a constant and the
//!   other asks the allocator, whose refusal is a null, so there is nothing in
//!   either to panic.
//! - **A refused allocation is not a panic.** Rust aborts when memory it needs
//!   for itself is refused — the copy of an input, a transcript that grew — and
//!   `catch_unwind` never sees an abort: natively it ends the process, in wasm
//!   it traps the instance. [`dev_alloc`] is the one place a refusal comes
//!   back, as a null, because there the host asked and can check.

mod slab;

use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::sync::Mutex;
use std::sync::MutexGuard;

use dev_core::Session;
use dev_protocol::Event;
use slab::Fault;
use slab::Slab;

/// The call did what it was asked.
pub const DEV_OK: i32 = 0;
/// The handle names no live session.
pub const DEV_HANDLE: i32 = -1;
/// The payload is not a message of this protocol.
pub const DEV_MALFORMED: i32 = -2;
/// A previous call on this session panicked.
pub const DEV_POISON: i32 = -3;
/// This call panicked; the session is now poisoned.
pub const DEV_PANIC: i32 = -4;
/// A pointer the caller must supply was null.
pub const DEV_NULL: i32 = -5;
/// The session is inside another call.
pub const DEV_BUSY: i32 = -6;
/// No slot is left for another session.
pub const DEV_FULL: i32 = -7;

/// A byte string Rust owns until [`dev_free`].
#[repr(C)]
pub struct Buf {
    pub ptr: *mut u8,
    pub len: usize,
    pub cap: usize,
}

impl Buf {
    /// A buffer that owns nothing — what a caller starts with.
    pub const fn empty() -> Self {
        Self {
            ptr: std::ptr::null_mut(),
            len: 0,
            cap: 0,
        }
    }
}

static SESSIONS: Mutex<Slab<Session>> = Mutex::new(Slab::new());

/// Version of the ABI and of the payload encoding behind it.
#[unsafe(no_mangle)]
pub extern "C" fn dev_abi() -> u32 {
    dev_protocol::ABI
}

/// Start a session from an encoded `Config`, or from the default when `len`
/// is zero. Writes the handle to `out`.
///
/// # Safety
/// `cfg` must be valid for `len` bytes unless `len` is zero, and `out` must
/// be writable. Both may be read only during the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dev_new(cfg: *const u8, len: usize, out: *mut u64) -> i32 {
    guard(|| {
        if out.is_null() {
            return DEV_NULL;
        }
        let config = if len == 0 {
            dev_protocol::Config::default()
        } else {
            let Some(bytes) = (unsafe { copy(cfg, len) }) else {
                return DEV_NULL;
            };
            match dev_protocol::decode(&bytes) {
                Ok(config) => config,
                Err(_) => return DEV_MALFORMED,
            }
        };
        match sessions().insert(Session::new(config)) {
            Ok(handle) => {
                unsafe { out.write(handle) };
                DEV_OK
            }
            Err(fault) => status(fault),
        }
    })
}

/// Feed one encoded `Event` and write the encoded `Vec<Action>` to `out`.
///
/// # Safety
/// `event` must be valid for `len` bytes unless `len` is zero, and `out` must
/// be writable. On [`DEV_OK`] `out` holds a buffer to release with
/// [`dev_free`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dev_step(
    session: u64,
    event: *const u8,
    len: usize,
    out: *mut Buf,
) -> i32 {
    guard(|| {
        if out.is_null() {
            return DEV_NULL;
        }
        let Some(bytes) = (unsafe { copy(event, len) }) else {
            return DEV_NULL;
        };
        let (status, answer) = enter(session, |session| {
            let event: Event = match dev_protocol::decode(&bytes) {
                Ok(event) => event,
                Err(_) => return (DEV_MALFORMED, Vec::new()),
            };
            let actions = session.step(event);
            // An action is integers, strings, options and enums, so there is
            // nothing in one a serializer refuses and this arm is dead for
            // today's types. Should one ever arrive, [`DEV_MALFORMED`] is about
            // the answer rather than the event, and the session has moved past
            // what the host can see: restore it from the last snapshot instead
            // of stepping it again.
            match dev_protocol::encode(&actions) {
                Ok(bytes) => (DEV_OK, bytes),
                Err(_) => (DEV_MALFORMED, Vec::new()),
            }
        });
        if status == DEV_OK {
            unsafe { out.write(lend(answer)) };
        }
        status
    })
}

/// Write the session's snapshot to `out`.
///
/// # Safety
/// `out` must be writable. On [`DEV_OK`] it holds a buffer to release with
/// [`dev_free`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dev_snapshot(session: u64, out: *mut Buf) -> i32 {
    guard(|| {
        if out.is_null() {
            return DEV_NULL;
        }
        // Same as `dev_step`: a session holds nothing a serializer refuses, so
        // the refusal arm is about the snapshot rather than any input.
        let (status, state) = enter(session, |session| match session.snapshot() {
            Ok(bytes) => (DEV_OK, bytes),
            Err(_) => (DEV_MALFORMED, Vec::new()),
        });
        if status == DEV_OK {
            unsafe { out.write(lend(state)) };
        }
        status
    })
}

/// Rebuild a session from a snapshot and write its new handle to `out`.
///
/// [`DEV_OK`] proves the bytes are intact, not that the sequence agrees with
/// the host's journal: a snapshot older than the journal mints again ids the
/// host has already dispatched under. Reconcile the ids this session answers
/// with against the journal before dispatching one — see
/// [`dev_protocol::pack`].
///
/// # Safety
/// `state` must be valid for `len` bytes unless `len` is zero, and `out` must
/// be writable.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dev_restore(state: *const u8, len: usize, out: *mut u64) -> i32 {
    guard(|| {
        if out.is_null() {
            return DEV_NULL;
        }
        let Some(bytes) = (unsafe { copy(state, len) }) else {
            return DEV_NULL;
        };
        let session = match Session::restore(&bytes) {
            Ok(session) => session,
            Err(_) => return DEV_MALFORMED,
        };
        match sessions().insert(session) {
            Ok(handle) => {
                unsafe { out.write(handle) };
                DEV_OK
            }
            Err(fault) => status(fault),
        }
    })
}

/// End a session and retire its handle.
#[unsafe(no_mangle)]
pub extern "C" fn dev_drop(session: u64) {
    guard(|| {
        let _ = sessions().remove(session);
        DEV_OK
    });
}

/// Release a `Buf` this library handed out.
///
/// # Safety
/// `buf` must be one this library returned and not yet released.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dev_free(buf: Buf) {
    guard(|| {
        if !buf.ptr.is_null() {
            drop(unsafe { Vec::from_raw_parts(buf.ptr, buf.len, buf.cap) });
        }
        DEV_OK
    });
}

/// Lend the host `len` bytes inside this library's memory.
///
/// The loop compiles to wasm unchanged, and there the host and the library do
/// not share an address space: every input pointer and every out-parameter has
/// to name memory that is IN the module, which the host cannot produce for
/// itself. This is how it gets some. The bytes are zeroed, exactly `len` long,
/// aligned for an out-parameter as well as for bytes, and the host's until
/// [`dev_release`].
///
/// Null for a zero `len` or when the allocator refuses — a host that asks for
/// nothing is given nothing to release.
#[unsafe(no_mangle)]
pub extern "C" fn dev_alloc(len: usize) -> *mut u8 {
    if len == 0 {
        return std::ptr::null_mut();
    }
    // The allocator is asked directly. A `vec!` answers a refusal by aborting
    // the process, which is not a panic, so no `catch_unwind` ever sees it; a
    // length with no layout at all is refused here instead.
    let Some(layout) = lent(len) else {
        return std::ptr::null_mut();
    };
    // SAFETY: `len` is not zero, so neither is the layout.
    unsafe { std::alloc::alloc_zeroed(layout) }
}

/// Take back a buffer [`dev_alloc`] lent.
///
/// Both ends work the layout out from `len` alone, so the pair is exact: the
/// allocation is `len` bytes and is released as `len` bytes, with no capacity
/// for the host to remember or get wrong.
///
/// # Safety
/// `ptr` and `len` must be one [`dev_alloc`] call's answer and its argument,
/// not yet released.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dev_release(ptr: *mut u8, len: usize) {
    guard(|| {
        if !ptr.is_null()
            && len != 0
            && let Some(layout) = lent(len)
        {
            unsafe { std::alloc::dealloc(ptr, layout) };
        }
        DEV_OK
    });
}

/// The layout of `len` lent bytes, if a length that long has one.
///
/// A host that borrows its memory borrows its out-parameters too, and the
/// library writes a `u64` handle or a [`Buf`] into them in place. So a loan is
/// aligned for both, whatever its length: a byte's alignment would leave where
/// they land to the allocator's habits.
fn lent(len: usize) -> Option<std::alloc::Layout> {
    std::alloc::Layout::from_size_align(len, align_of::<(u64, Buf)>()).ok()
}

/// Run a call with the session lifted out of the slab, so no lock is held
/// while it runs and a panic leaves the slot poisoned rather than occupied.
///
/// The bytes the call produced reach the caller only once the session is back
/// in its slot. A [`dev_drop`] that lands in between retires the handle, the
/// slab refuses the session, and the call answers [`DEV_HANDLE`] with nothing:
/// a host that has dropped a session is never handed actions it would have to
/// account for against one.
fn enter(handle: u64, call: impl FnOnce(&mut Session) -> (i32, Vec<u8>)) -> (i32, Vec<u8>) {
    let mut session = match sessions().take(handle) {
        Ok(session) => session,
        Err(fault) => return (status(fault), Vec::new()),
    };
    match catch_unwind(AssertUnwindSafe(|| call(&mut session))) {
        Ok(answer) => match sessions().put(handle, session) {
            Ok(()) => answer,
            // The slab handed the session back because the handle is retired.
            // It dies here, with its answer, which is what dropping it meant.
            Err(_) => (DEV_HANDLE, Vec::new()),
        },
        Err(_) => {
            drop(session);
            sessions().poison(handle);
            (DEV_PANIC, Vec::new())
        }
    }
}

/// Keep a panic inside Rust. An entry point that has no session to poison
/// still answers rather than unwinding into C.
fn guard(call: impl FnOnce() -> i32) -> i32 {
    match catch_unwind(AssertUnwindSafe(call)) {
        Ok(status) => status,
        Err(_) => DEV_PANIC,
    }
}

fn sessions() -> MutexGuard<'static, Slab<Session>> {
    // The lock is held only for slab bookkeeping; no session code runs under
    // it, so the only way it is poisoned is a panic in the slab itself, and
    // the slab is still consistent in that case.
    match SESSIONS.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    }
}

fn status(fault: Fault) -> i32 {
    match fault {
        Fault::Stale => DEV_HANDLE,
        Fault::Full => DEV_FULL,
        Fault::Poisoned => DEV_POISON,
        Fault::Busy => DEV_BUSY,
    }
}

/// Copy the caller's bytes before anything else touches them.
///
/// # Safety
/// `ptr` must be valid for `len` bytes, or `len` must be zero.
unsafe fn copy(ptr: *const u8, len: usize) -> Option<Vec<u8>> {
    if len == 0 {
        return Some(Vec::new());
    }
    if ptr.is_null() {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec())
}

/// Hand a Rust allocation to C. It comes back through [`dev_free`].
fn lend(mut bytes: Vec<u8>) -> Buf {
    let buf = Buf {
        ptr: bytes.as_mut_ptr(),
        len: bytes.len(),
        cap: bytes.capacity(),
    };
    std::mem::forget(bytes);
    buf
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use dev_protocol::Action;
    use dev_protocol::Config;
    use dev_protocol::Op;
    use dev_protocol::Turn;
    use std::sync::atomic::AtomicU64;
    use std::sync::atomic::Ordering;

    fn start() -> u64 {
        let mut handle = 0u64;
        assert_eq!(
            unsafe { dev_new(std::ptr::null(), 0, &raw mut handle) },
            DEV_OK
        );
        assert_ne!(handle, 0);
        handle
    }

    fn step(handle: u64, event: &Event) -> Result<Vec<Action>, i32> {
        let bytes = dev_protocol::encode(event).expect("encode");
        let mut out = Buf::empty();
        let status = unsafe { dev_step(handle, bytes.as_ptr(), bytes.len(), &raw mut out) };
        if status != DEV_OK {
            assert!(out.ptr.is_null(), "a refused call lent a buffer out");
            return Err(status);
        }
        let actions = dev_protocol::decode(unsafe {
            std::slice::from_raw_parts(out.ptr.cast_const(), out.len)
        })
        .expect("decode actions");
        unsafe { dev_free(out) };
        Ok(actions)
    }

    fn turn(prompt: &str) -> Event {
        // Each turn needs an id the session has not accepted before; a session
        // refuses a redelivery, which is the point of the id.
        static NEXT: AtomicU64 = AtomicU64::new(1);
        Event::Turn(Turn {
            id: NEXT.fetch_add(1, Ordering::Relaxed),
            prompt: prompt.to_string(),
        })
    }

    #[test]
    fn the_abi_is_the_protocol_version() {
        assert_eq!(dev_abi(), dev_protocol::ABI);
    }

    #[test]
    fn a_step_round_trips_through_the_boundary() {
        let handle = start();
        let actions = step(handle, &turn("fix the build")).expect("step");
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].id, 1);
        assert!(matches!(actions[0].op, Op::Model(_)));
        dev_drop(handle);
    }

    #[test]
    fn a_config_is_decoded_and_garbage_is_refused() {
        let cfg = dev_protocol::encode(&Config {
            model: Some("zen5.8".to_string()),
            prelude: Some("you are a coding agent".to_string()),
        })
        .expect("encode");
        let mut handle = 0u64;
        assert_eq!(
            unsafe { dev_new(cfg.as_ptr(), cfg.len(), &raw mut handle) },
            DEV_OK
        );
        let actions = step(handle, &turn("fix the build")).expect("step");
        match &actions[0].op {
            Op::Model(ask) => {
                assert_eq!(ask.model.as_deref(), Some("zen5.8"));
                assert_eq!(ask.prelude.as_deref(), Some("you are a coding agent"));
                assert_eq!(ask.messages.len(), 1);
            }
            op => panic!("asked for {op:?}"),
        }
        dev_drop(handle);

        let junk = b"not a config";
        assert_eq!(
            unsafe { dev_new(junk.as_ptr(), junk.len(), &raw mut handle) },
            DEV_MALFORMED
        );
    }

    #[test]
    fn a_stale_handle_is_refused() {
        let handle = start();
        dev_drop(handle);
        assert_eq!(step(handle, &turn("fix the build")), Err(DEV_HANDLE));

        let mut out = Buf::empty();
        assert_eq!(
            unsafe { dev_snapshot(handle, &raw mut out) },
            DEV_HANDLE,
            "a snapshot of a dropped session"
        );
        // A handle from a different slab generation is refused as well.
        let next = start();
        assert_ne!(next, handle);
        assert_eq!(step(handle, &turn("fix the build")), Err(DEV_HANDLE));
        dev_drop(next);
    }

    #[test]
    fn a_panic_poisons_the_session_and_leaves_the_process_up() {
        let handle = start();
        let quiet = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let (status, answer) = enter(handle, |_| panic!("the loop gave up"));
        std::panic::set_hook(quiet);
        assert_eq!(status, DEV_PANIC);
        assert!(answer.is_empty());

        assert_eq!(step(handle, &turn("fix the build")), Err(DEV_POISON));
        let mut out = Buf::empty();
        assert_eq!(unsafe { dev_snapshot(handle, &raw mut out) }, DEV_POISON);

        // Another session is unharmed, and the poisoned handle frees cleanly.
        let other = start();
        assert!(step(other, &turn("fix the build")).is_ok());
        dev_drop(other);
        dev_drop(handle);
        assert_eq!(step(handle, &turn("fix the build")), Err(DEV_HANDLE));
    }

    #[test]
    fn a_malformed_event_is_refused_without_poisoning() {
        let handle = start();
        let junk = b"not an event";
        let mut out = Buf::empty();
        assert_eq!(
            unsafe { dev_step(handle, junk.as_ptr(), junk.len(), &raw mut out) },
            DEV_MALFORMED
        );
        assert!(step(handle, &turn("fix the build")).is_ok());
        dev_drop(handle);
    }

    #[test]
    fn a_null_out_pointer_is_refused() {
        let handle = start();
        let bytes = dev_protocol::encode(&turn("fix the build")).expect("encode");
        assert_eq!(
            unsafe { dev_step(handle, bytes.as_ptr(), bytes.len(), std::ptr::null_mut()) },
            DEV_NULL
        );
        let mut out = Buf::empty();
        assert_eq!(
            unsafe { dev_step(handle, std::ptr::null(), 12, &raw mut out) },
            DEV_NULL
        );
        dev_drop(handle);
    }

    #[test]
    fn a_restored_session_continues_the_sequence() {
        let handle = start();
        let first = step(handle, &turn("fix the build")).expect("step");

        let mut state = Buf::empty();
        assert_eq!(unsafe { dev_snapshot(handle, &raw mut state) }, DEV_OK);
        let mut restored = 0u64;
        assert_eq!(
            unsafe { dev_restore(state.ptr.cast_const(), state.len, &raw mut restored) },
            DEV_OK
        );
        unsafe { dev_free(state) };
        assert_ne!(restored, handle);

        let answer = Event::Model(dev_protocol::Answer {
            id: first[0].id,
            reply: dev_protocol::Reply {
                text: Some("built".to_string()),
                calls: Vec::new(),
            },
        });
        let here = step(handle, &answer).expect("step");
        let there = step(restored, &answer).expect("step");
        assert_eq!(here, there);
        assert_eq!(there[0].id, first[0].id + 1);

        dev_drop(handle);
        dev_drop(restored);
    }

    #[test]
    fn an_edited_snapshot_is_refused() {
        let handle = start();
        step(handle, &turn("fix the build")).expect("step");
        let mut state = Buf::empty();
        assert_eq!(unsafe { dev_snapshot(handle, &raw mut state) }, DEV_OK);
        let mut bytes =
            unsafe { std::slice::from_raw_parts(state.ptr.cast_const(), state.len) }.to_vec();
        unsafe { dev_free(state) };
        dev_drop(handle);

        let body = String::from_utf8(bytes.split_off(12)).expect("a json body");
        let forged = body.replace("\"next\":2", "\"next\":1");
        assert_ne!(forged, body, "the snapshot no longer spells the next id");
        bytes.extend_from_slice(forged.as_bytes());
        let mut restored = 0u64;
        assert_eq!(
            unsafe { dev_restore(bytes.as_ptr(), bytes.len(), &raw mut restored) },
            DEV_MALFORMED
        );
        assert_eq!(restored, 0);
    }

    /// The window `enter` opens on purpose: the lock is not held while the
    /// session runs, so a `dev_drop` can land in the middle of a step.
    #[test]
    fn a_drop_during_a_step_answers_for_a_handle_that_is_gone() {
        let handle = start();
        let (status, answer) = enter(handle, |session| {
            dev_drop(handle);
            let actions = session.step(turn("fix the build"));
            assert_eq!(actions.len(), 1, "the session did step");
            (DEV_OK, dev_protocol::encode(&actions).expect("encode"))
        });
        assert_eq!(status, DEV_HANDLE, "a lost session answered OK");
        assert!(answer.is_empty(), "actions for a session that is gone");
        assert_eq!(step(handle, &turn("fix the build")), Err(DEV_HANDLE));
    }

    /// Two threads, one racing `dev_drop` against the other's `dev_step`, over
    /// and over. This is the slab under contention: take, put, remove and the
    /// free list all interleaving. It pins the answers — only [`DEV_OK`] or
    /// [`DEV_HANDLE`], a buffer only on `DEV_OK`, a live session for anything
    /// that claims one — and it is not what pins the lost-session fix, because
    /// a handle is dead afterwards either way. That is
    /// `a_drop_during_a_step_answers_for_a_handle_that_is_gone`, which times the
    /// drop inside the window instead of racing for it.
    #[test]
    fn a_race_between_a_step_and_a_drop_never_reports_a_lost_session() {
        for _ in 0..500 {
            let handle = start();
            let stepping = std::thread::spawn(move || step(handle, &turn("fix the build")));
            let dropping = std::thread::spawn(move || dev_drop(handle));
            let stepped = stepping.join().expect("stepping thread");
            dropping.join().expect("dropping thread");

            match stepped {
                Ok(actions) => {
                    assert_eq!(actions.len(), 1);
                    assert!(matches!(actions[0].op, Op::Model(_)));
                    // The step won the race, so its session must still be there
                    // for the host to snapshot — or already dropped by the
                    // other thread, never a slot handed to someone else.
                    let mut out = Buf::empty();
                    let status = unsafe { dev_snapshot(handle, &raw mut out) };
                    assert!(
                        status == DEV_OK || status == DEV_HANDLE,
                        "snapshot answered {status}"
                    );
                    if status == DEV_OK {
                        unsafe { dev_free(out) };
                    }
                }
                Err(status) => assert_eq!(status, DEV_HANDLE, "the step answered {status}"),
            }
            dev_drop(handle);
        }
    }

    /// A host with no memory of its own drives a whole step through borrowed
    /// buffers: the input, and both out-parameters, live inside the library.
    #[test]
    fn a_host_that_shares_no_memory_steps_through_borrowed_buffers() {
        let bytes = dev_protocol::encode(&Event::Turn(Turn {
            id: 1,
            prompt: "fix the build".into(),
        }))
        .expect("encode");

        let handle_slot = dev_alloc(std::mem::size_of::<u64>());
        let input = dev_alloc(bytes.len());
        let out_slot = dev_alloc(std::mem::size_of::<Buf>());
        assert!(!handle_slot.is_null() && !input.is_null() && !out_slot.is_null());

        unsafe {
            assert_eq!(dev_new(std::ptr::null(), 0, handle_slot.cast::<u64>()), DEV_OK);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), input, bytes.len());
            let handle = handle_slot.cast::<u64>().read_unaligned();
            assert_eq!(
                dev_step(handle, input, bytes.len(), out_slot.cast::<Buf>()),
                DEV_OK
            );
            let out = out_slot.cast::<Buf>().read_unaligned();
            let actions: Vec<Action> =
                dev_protocol::decode(std::slice::from_raw_parts(out.ptr.cast_const(), out.len))
                    .expect("decode actions");
            assert!(
                actions.iter().any(|a| matches!(a.op, Op::Model(_))),
                "a first turn asks the model: {actions:?}"
            );
            dev_free(out);
            dev_drop(handle);
            dev_release(out_slot, std::mem::size_of::<Buf>());
            dev_release(input, bytes.len());
            dev_release(handle_slot, std::mem::size_of::<u64>());
        }
    }

    /// What is lent is zeroed and exactly as long as asked, so a host that
    /// writes fewer bytes than it borrowed leaks nothing of a previous tenant.
    #[test]
    fn a_lent_buffer_is_zeroed_and_exact() {
        let ptr = dev_alloc(64);
        assert!(!ptr.is_null());
        let lent = unsafe { std::slice::from_raw_parts(ptr, 64) };
        assert!(lent.iter().all(|b| *b == 0));
        unsafe { dev_release(ptr, 64) };
    }

    /// A host with no memory of its own borrows its out-parameters too, so what
    /// is lent is aligned for them at every length: a handle is a `u64` and an
    /// answer is a `Buf`, and the library writes both in place.
    #[test]
    fn a_lent_buffer_is_aligned_for_an_out_parameter() {
        for len in 1..=64 {
            let ptr = dev_alloc(len);
            assert!(!ptr.is_null());
            assert!(
                ptr.cast::<u64>().is_aligned() && ptr.cast::<Buf>().is_aligned(),
                "{len} bytes were lent at {ptr:p}"
            );
            unsafe { dev_release(ptr, len) };
        }
    }

    /// No allocator has the longest loan that still has a layout, so that
    /// refusal is the allocator's own: the one that aborts a process rather
    /// than unwinding. The lengths past it have no layout at all. Every one of
    /// them is a null the host can check.
    #[test]
    fn a_refused_allocation_is_null_rather_than_an_abort() {
        let most = isize::MAX.unsigned_abs();
        let longest = most - (align_of::<(u64, Buf)>() - 1);
        assert!(lent(longest).is_some() && lent(longest + 1).is_none());
        for len in [longest, longest + 1, most + 1, usize::MAX] {
            assert!(dev_alloc(len).is_null(), "{len} bytes were lent");
        }
    }

    /// Asking for nothing lends nothing, and giving nothing back is not a fault.
    #[test]
    fn nothing_is_lent_for_nothing() {
        assert!(dev_alloc(0).is_null());
        unsafe { dev_release(std::ptr::null_mut(), 0) };
        unsafe { dev_release(std::ptr::null_mut(), 16) };
    }
}
