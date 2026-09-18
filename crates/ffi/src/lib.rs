//! The C ABI the Go host drives the loop through.
//!
//! Seven symbols, an opaque `u64` per session, and a `Buf` for every byte
//! string that crosses back. The rules:
//!
//! - **Rust copies every input** before the call returns, so the host may free
//!   or reuse its buffer immediately.
//! - **Rust owns every output** until the host calls [`dev_free`]. A `Buf` is
//!   a Rust allocation; `free(3)` on it is wrong.
//! - **A handle is generational.** Dropping a session retires its handle, and
//!   a stale handle is refused with [`DEV_HANDLE`] rather than resolving to
//!   whatever took the slot next.
//! - **A panic does not cross the boundary.** Every entry point runs inside
//!   `catch_unwind`; an escaped panic answers [`DEV_PANIC`] and poisons that
//!   session, whose every later call answers [`DEV_POISON`] until it is
//!   dropped. The process stays up and the other sessions are untouched.

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
        enter(session, |session| {
            let event: Event = match dev_protocol::decode(&bytes) {
                Ok(event) => event,
                Err(_) => return DEV_MALFORMED,
            };
            let actions = session.step(event);
            unsafe { out.write(lend(dev_protocol::encode(&actions))) };
            DEV_OK
        })
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
        enter(session, |session| {
            unsafe { out.write(lend(session.snapshot())) };
            DEV_OK
        })
    })
}

/// Rebuild a session from a snapshot and write its new handle to `out`.
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

/// Run a call with the session lifted out of the slab, so no lock is held
/// while it runs and a panic leaves the slot poisoned rather than occupied.
fn enter(handle: u64, call: impl FnOnce(&mut Session) -> i32) -> i32 {
    let mut session = match sessions().take(handle) {
        Ok(session) => session,
        Err(fault) => return status(fault),
    };
    match catch_unwind(AssertUnwindSafe(|| call(&mut session))) {
        Ok(status) => {
            sessions().put(handle, session);
            status
        }
        Err(_) => {
            drop(session);
            sessions().poison(handle);
            DEV_PANIC
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
        let bytes = dev_protocol::encode(event);
        let mut out = Buf::empty();
        let status = unsafe { dev_step(handle, bytes.as_ptr(), bytes.len(), &raw mut out) };
        if status != DEV_OK {
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
        Event::Turn(Turn {
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
        });
        let mut handle = 0u64;
        assert_eq!(
            unsafe { dev_new(cfg.as_ptr(), cfg.len(), &raw mut handle) },
            DEV_OK
        );
        let actions = step(handle, &turn("fix the build")).expect("step");
        match &actions[0].op {
            Op::Model(ask) => {
                assert_eq!(ask.model.as_deref(), Some("zen5.8"));
                assert_eq!(ask.messages.len(), 2);
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
        let status = enter(handle, |_| panic!("the loop gave up"));
        std::panic::set_hook(quiet);
        assert_eq!(status, DEV_PANIC);

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
        let bytes = dev_protocol::encode(&turn("fix the build"));
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
}
