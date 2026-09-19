/* The C ABI of the dev agent loop (HIP-1330).
 *
 * Written by hand and kept beside crates/ffi/src/lib.rs, which the C ABI test
 * in crates/ffi/tests/abi.c compiles against the staticlib. There is no
 * generator step to forget to run.
 *
 * Ownership:
 *   - Every pointer you pass in is copied before the call returns.
 *   - Every dev_buf handed back is a Rust allocation; release it with
 *     dev_free and never with free(3).
 *   - A session handle is opaque and generational: dev_drop retires it, and a
 *     retired handle answers DEV_HANDLE instead of naming a live session. A
 *     dev_drop that lands while a call on that session is running makes the
 *     call answer DEV_HANDLE and hand back no dev_buf: there is no session
 *     left for its answer to belong to.
 *   - A call that panics answers DEV_PANIC and poisons that session; its later
 *     calls answer DEV_POISON until dev_drop. Other sessions are untouched.
 *   - A refused allocation is not a panic. Memory the library needs for itself
 *     and is refused ends the process, or in wasm traps the instance; dev_alloc
 *     is the one place a refusal comes back, as a null.
 *   - A dev_buf is written only on DEV_OK. On any other status the out
 *     parameter is left as the caller passed it.
 *
 * Payloads are dev-protocol messages (Config, Event, Action list, snapshot) in
 * the encoding dev_abi() names.
 */

#ifndef HANZO_DEV_H
#define HANZO_DEV_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DEV_OK 0         /* the call did what it was asked */
#define DEV_HANDLE (-1)  /* the handle names no live session */
#define DEV_MALFORMED (-2) /* the payload is not a message of this protocol */
#define DEV_POISON (-3)  /* a previous call on this session panicked */
#define DEV_PANIC (-4)   /* this call panicked; the session is now poisoned */
#define DEV_NULL (-5)    /* a pointer the caller must supply was null */
#define DEV_BUSY (-6)    /* the session is inside another call */
#define DEV_FULL (-7)    /* no slot is left for another session */

/* A byte string Rust owns until dev_free. */
typedef struct dev_buf {
  uint8_t *ptr;
  size_t len;
  size_t cap;
} dev_buf;

/* Version of this ABI and of the payload encoding behind it. */
uint32_t dev_abi(void);

/* Start a session from an encoded Config, or from the default when len is 0.
 * Writes the handle to out. */
int32_t dev_new(const uint8_t *cfg, size_t len, uint64_t *out);

/* Feed one encoded Event; writes the encoded action list to out. */
int32_t dev_step(uint64_t session, const uint8_t *event, size_t len, dev_buf *out);

/* Write the session's snapshot to out. */
int32_t dev_snapshot(uint64_t session, dev_buf *out);

/* Rebuild a session from a snapshot; writes its new handle to out. DEV_OK
 * proves the bytes are intact, not that the sequence agrees with the host's
 * journal: a snapshot older than the journal mints again ids the host has
 * already dispatched under. Reconcile the ids this session answers with
 * against the journal before dispatching one (see `pack` in dev-protocol). */
int32_t dev_restore(const uint8_t *state, size_t len, uint64_t *out);

/* End a session and retire its handle. */
void dev_drop(uint64_t session);

/* Release a dev_buf this library handed out. */
void dev_free(dev_buf buf);
/* Lend the host len zeroed bytes inside this library's memory, aligned for a
 * uint64_t or a dev_buf as well as for bytes; null for a zero len or a refused
 * allocation. Compiled to wasm the library's memory is its own, so a host there
 * has no other way to produce an input pointer or an out-parameter. A native
 * host shares an address space and never needs it. */
uint8_t *dev_alloc(size_t len);
/* Take back what dev_alloc lent. ptr and len are that call's answer and its
 * argument. */
void dev_release(uint8_t *ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* HANZO_DEV_H */
