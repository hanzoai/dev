# Hanzo Connect setup

Deployment and client configuration for Hanzo Connect. The [architecture note](../internals/hanzo-connect.md)
explains the trust boundaries; the [relay README](../../infra/relay/README.md#deployment) owns relay
provisioning instructions.

Identity is Hanzo IAM. Sign-in starts at `https://hanzo.ai/login`; the issuer and keys are whatever `https://hanzo.ai/.well-known/openid-configuration` names. Clients sign in with the `hanzo-app` application and send
the access token to the relay, which verifies it against the issuer's JWKS.

## Public application configuration

Hanzo Connect is disabled in a fresh clone. To build against the production deployment, copy the
repository-root example:

```sh
cp .env.example .env
```

For another deployment, set these values in the repository-root `.env` or `.env.local`:

```dotenv
HANZO_DEV_RELAY_URL=https://relay.example.com
# Only when the deployment uses another IAM or application than Hanzo ID's `hanzo-app`.
HANZO_DEV_IAM_ISSUER=https://hanzo.ai
HANZO_DEV_IAM_CLIENT_ID=hanzo-app
```

Process variables take precedence over `.env.local`, then `.env`. Use these canonical names;
the build loader supplies framework-specific aliases. These values are public identifiers.
Client and bundled-server builds embed them, so set them before building. Bundled servers also
accept runtime overrides for operator-managed deployments.

Copy `infra/relay/.env.example` to `infra/relay/.env` for relay deployment settings.
Deploy `prod` before personal stages because it owns the retained database that their branches
depend on. The stack's `PublishClientConfig` action writes the resulting relay URL back to the root `.env`.

## Hanzo IAM application

In Hanzo IAM, the `hanzo-app` application must:

1. Be a public client using authorization code with PKCE.
2. Allow the redirect URIs `http://127.0.0.1:34338/callback` (CLI loopback), `hanzo-dev://app/`
   and `hanzodev://app/` (desktop; development uses `hanzo-dev`, production `hanzodev`), and the
   hosted web origin.
3. Enable the `openid`, `profile`, `email`, and `offline_access` scopes.
4. Enable the device authorization grant. Headless and SSH authorization use it.

The relay accepts tokens whose `iss` is the `issuer` of the discovery document at `HANZO_IAM_ORIGIN` (default `https://hanzo.ai`) and whose
`aud` is `HANZO_IAM_AUDIENCE` (default `hanzo-app`). The relay needs no IAM secret: it only reads the
public JWKS. The audience stays the same across relay stages; the relay URL selects the deployment.

## Restricting sign-ups

Sign-up policy and bans are Hanzo IAM's. Disable the account in IAM when its active sessions and
future sign-ins must be revoked; the relay stops accepting its tokens when they expire.

## Desktop passkeys

Passkeys are an IAM feature; the desktop app signs in through the system browser and the redirect
URIs above. Configure signing as described in the [release runbook](./release.md#2-apple-signing--notarization-setup-macos).

Local signed builds additionally use:

```dotenv
HANZO_DEV_APPLE_TEAM_ID=ABC1234567
HANZO_DEV_MACOS_PROVISIONING_PROFILE=/absolute/path/to/hanzodev.provisionprofile
```

The ordinary `dev:desktop` launcher is unsigned. For renderer HMR, install a signed build, start
`vp run dev:web`, and launch the installed executable with the actual web and server ports. For
example, with the default ports:

```sh
VITE_DEV_SERVER_URL=http://127.0.0.1:5733 \
HANZO_DEV_PORT=13773 \
  "/Applications/Hanzo Dev (Alpha).app/Contents/MacOS/Hanzo Dev (Alpha)"
```

Rebuild the signed app after native dependency, main-process, preload, entitlement, provisioning,
or signing changes. Renderer edits can reuse it. Verify the installed bundle before testing:

```sh
codesign --verify --deep --strict "/Applications/Hanzo Dev (Alpha).app"
codesign -d --entitlements :- "/Applications/Hanzo Dev (Alpha).app"
```
