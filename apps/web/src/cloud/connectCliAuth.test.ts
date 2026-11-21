import { afterEach, describe, expect, it, vi } from "vite-plus/test";

import { buildConnectCliAuthorizeUrl, connectCliSignInRedirectUrl } from "./connectCliAuth";

describe("connectCliAuth", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("builds a PKCE authorize URL that redirects to the CLI's loopback listener", () => {
    vi.stubEnv("VITE_HANZO_DEV_IAM_ISSUER", "https://hanzo.id");
    vi.stubEnv("VITE_HANZO_DEV_IAM_CLIENT_ID", "hanzo-app");

    const authorizeUrl = buildConnectCliAuthorizeUrl({
      state: "state-1",
      challenge: "challenge-1",
      loopbackPort: 34338,
    });
    expect(authorizeUrl).not.toBeNull();

    const url = new URL(authorizeUrl!);
    expect(url.hostname).toBe("hanzo.id");
    expect(url.pathname).toBe("/oauth/authorize");
    expect(url.searchParams.get("client_id")).toBe("hanzo-app");
    expect(url.searchParams.get("redirect_uri")).toBe("http://127.0.0.1:34338/callback");
    expect(url.searchParams.get("state")).toBe("state-1");
    expect(url.searchParams.get("code_challenge")).toBe("challenge-1");
    expect(url.searchParams.get("code_challenge_method")).toBe("S256");
  });

  it("sends the sign-in redirect to the authorize endpoint, not back to /connect", () => {
    const connectUrl = "https://hanzo.bot/connect#state=state-1&challenge=challenge-1&port=34338";
    const redirectUrl = connectCliSignInRedirectUrl(
      { state: "state-1", challenge: "challenge-1", loopbackPort: 34338 },
      connectUrl,
    );

    expect(redirectUrl).not.toBe(connectUrl);
    expect(new URL(redirectUrl).pathname).toBe("/oauth/authorize");
  });
});
