/* The ABI, exercised the way the Go host will exercise it: a session, a turn,
 * a snapshot, a restore, a cancel, and a handle that has been dropped.
 *
 * The event and the expected substrings below are in the protocol's current
 * encoding (dev-protocol `encode`). When that encoding changes, this fixture
 * changes with it and no declaration in dev.h moves.
 */

#include "dev.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures = 0;

#define CHECK(cond)                                                            \
  do {                                                                         \
    if (!(cond)) {                                                             \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #cond);               \
      failures++;                                                              \
    }                                                                          \
  } while (0)

static const char *TURN = "{\"Turn\":{\"id\":1,\"prompt\":\"fix the build\"}}";

/* The buffer Rust lends is not terminated; copy it before reading it as text. */
static char *text(dev_buf buf) {
  char *copy = malloc(buf.len + 1);
  if (copy == NULL) {
    return NULL;
  }
  memcpy(copy, buf.ptr, buf.len);
  copy[buf.len] = '\0';
  return copy;
}

int main(void) {
  CHECK(dev_abi() == 1);

  uint64_t session = 0;
  CHECK(dev_new(NULL, 0, &session) == DEV_OK);
  CHECK(session != 0);

  dev_buf actions = {0};
  CHECK(dev_step(session, (const uint8_t *)TURN, strlen(TURN), &actions) == DEV_OK);
  CHECK(actions.ptr != NULL);
  CHECK(actions.len > 0);
  char *shown = text(actions);
  CHECK(shown != NULL);
  if (shown != NULL) {
    /* One action, id 1, asking for a model. */
    CHECK(strstr(shown, "\"id\":1") != NULL);
    CHECK(strstr(shown, "Model") != NULL);
    free(shown);
  }
  dev_free(actions);

  /* The same turn again is a redelivery: no action, and the prompt is not
   * asked a second time. */
  dev_buf twice = {0};
  CHECK(dev_step(session, (const uint8_t *)TURN, strlen(TURN), &twice) == DEV_OK);
  shown = text(twice);
  CHECK(shown != NULL);
  if (shown != NULL) {
    CHECK(strcmp(shown, "[]") == 0);
    free(shown);
  }
  dev_free(twice);

  /* A snapshot restores into a second session that continues the sequence. */
  dev_buf state = {0};
  CHECK(dev_snapshot(session, &state) == DEV_OK);
  CHECK(state.len > 0);
  uint64_t restored = 0;
  CHECK(dev_restore(state.ptr, state.len, &restored) == DEV_OK);
  CHECK(restored != 0);
  CHECK(restored != session);
  dev_free(state);

  const char *answer =
      "{\"Model\":{\"id\":1,\"reply\":{\"text\":\"built\",\"calls\":[]}}}";
  dev_buf ended = {0};
  CHECK(dev_step(restored, (const uint8_t *)answer, strlen(answer), &ended) == DEV_OK);
  shown = text(ended);
  CHECK(shown != NULL);
  if (shown != NULL) {
    /* Emit, Save, then the Done that names the turn that asked. */
    CHECK(strstr(shown, "\"id\":2") != NULL);
    CHECK(strstr(shown, "\"id\":4") != NULL);
    CHECK(strstr(shown, "{\"Done\":{\"turn\":1,\"outcome\":\"Complete\"}}") != NULL);
    free(shown);
  }
  dev_free(ended);
  dev_drop(restored);

  /* Garbage is refused, the session survives it, and nothing is lent out. */
  dev_buf refused = {0};
  CHECK(dev_step(session, (const uint8_t *)"garbage", 7, &refused) == DEV_MALFORMED);
  CHECK(refused.ptr == NULL);

  /* A snapshot the store handed back changed is refused, not restored. */
  dev_buf tampered = {0};
  CHECK(dev_snapshot(session, &tampered) == DEV_OK);
  CHECK(tampered.len > 13);
  tampered.ptr[tampered.len - 1] ^= 0x20;
  uint64_t forged = 0;
  CHECK(dev_restore(tampered.ptr, tampered.len, &forged) == DEV_MALFORMED);
  CHECK(forged == 0);
  dev_free(tampered);

  /* A cancel names the turn it stops, so the same one delivered again stops
   * nothing. */
  const char *cancel = "{\"Cancel\":{\"turn\":1}}";
  dev_buf stopped = {0};
  CHECK(dev_step(session, (const uint8_t *)cancel, strlen(cancel), &stopped) == DEV_OK);
  shown = text(stopped);
  CHECK(shown != NULL);
  if (shown != NULL) {
    CHECK(strstr(shown, "{\"Done\":{\"turn\":1,\"outcome\":\"Cancelled\"}}") != NULL);
    free(shown);
  }
  dev_free(stopped);
  dev_buf again = {0};
  CHECK(dev_step(session, (const uint8_t *)cancel, strlen(cancel), &again) == DEV_OK);
  shown = text(again);
  CHECK(shown != NULL);
  if (shown != NULL) {
    CHECK(strcmp(shown, "[]") == 0);
    free(shown);
  }
  dev_free(again);

  /* A dropped handle names nothing. */
  dev_drop(session);
  CHECK(dev_step(session, (const uint8_t *)TURN, strlen(TURN), &refused) == DEV_HANDLE);
  CHECK(dev_snapshot(session, &refused) == DEV_HANDLE);
  CHECK(dev_new((const uint8_t *)"garbage", 7, &session) == DEV_MALFORMED);
  CHECK(dev_step(0, (const uint8_t *)TURN, strlen(TURN), &refused) == DEV_HANDLE);
  CHECK(dev_step(session, (const uint8_t *)TURN, strlen(TURN), NULL) == DEV_NULL);

  /* What dev_alloc lends comes back through dev_release, and an allocation no
   * allocator can make is a null rather than the end of the process. */
  uint8_t *lent = dev_alloc(16);
  CHECK(lent != NULL);
  dev_release(lent, 16);
  CHECK(dev_alloc(PTRDIFF_MAX) == NULL);

  if (failures != 0) {
    fprintf(stderr, "%d check(s) failed\n", failures);
    return 1;
  }
  printf("ok\n");
  return 0;
}
