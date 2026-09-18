/* The ABI, exercised the way the Go host will exercise it: a session, a turn,
 * a snapshot, a restore, and a handle that has been dropped.
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

static const char *TURN = "{\"Turn\":{\"prompt\":\"fix the build\"}}";

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
    CHECK(strstr(shown, "\"id\":2") != NULL);
    CHECK(strstr(shown, "Done") != NULL);
    free(shown);
  }
  dev_free(ended);
  dev_drop(restored);

  /* Garbage is refused, and the session survives it. */
  dev_buf refused = {0};
  CHECK(dev_step(session, (const uint8_t *)"garbage", 7, &refused) == DEV_MALFORMED);

  /* A dropped handle names nothing. */
  dev_drop(session);
  CHECK(dev_step(session, (const uint8_t *)TURN, strlen(TURN), &refused) == DEV_HANDLE);
  CHECK(dev_snapshot(session, &refused) == DEV_HANDLE);
  CHECK(dev_new((const uint8_t *)"garbage", 7, &session) == DEV_MALFORMED);
  CHECK(dev_step(0, (const uint8_t *)TURN, strlen(TURN), &refused) == DEV_HANDLE);
  CHECK(dev_step(session, (const uint8_t *)TURN, strlen(TURN), NULL) == DEV_NULL);

  if (failures != 0) {
    fprintf(stderr, "%d check(s) failed\n", failures);
    return 1;
  }
  printf("ok\n");
  return 0;
}
