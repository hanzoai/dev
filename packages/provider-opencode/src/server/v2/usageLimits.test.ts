import { assert, describe, it } from "@effect/vitest";

import { usageFailureMessage } from "./usageLimits.ts";

describe("usageFailureMessage", () => {
  it("says what went wrong instead of one message for every failure", () => {
    assert.match(usageFailureMessage({ _tag: "TimeoutError" }), /did not answer in time/);
    assert.match(usageFailureMessage({ response: { status: 401 } }), /sign-in expired/);
    assert.strictEqual(
      usageFailureMessage(new Error("boom")),
      "OpenCode could not read workspace usage.",
    );
  });
});
