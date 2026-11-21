import { describe, expect, it } from "vite-plus/test";

import { resolveSignInProps } from "./authRedirect";

describe("resolveSignInProps", () => {
  it("removes a sign-in virtual pathname and callback params while preserving the desktop route", () => {
    expect(
      resolveSignInProps(
        "hanzodev://app/AUTH-ROUTER/VIRTUAL/sign-up?status=complete#/settings/connections",
        true,
      ),
    ).toEqual({
      forceRedirectUrl: "hanzodev://app/#/settings/connections",
      signUpForceRedirectUrl: "hanzodev://app/#/settings/connections",
    });
  });
});
