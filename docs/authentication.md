# Authentication

## Signing in

Dev talks to the Hanzo gateway (`api.hanzo.ai/v1`) by default. Sign in once and
the machine is linked to your account:

```shell
hanzo auth login
```

That fills in `HANZO_USER_KEY`. You can also set it yourself from a key minted at
https://hanzo.ai/settings/keys. A service that runs Dev unattended sets
`HANZO_API_KEY` or `HANZO_MACHINE_TOKEN` instead.

## Using the `openai` provider

Everything below applies only when you point `model_provider` at `openai` — see
[Configuration](./config.md#model_providers).

Authenticate with an API key:

```shell
export OPENAI_API_KEY="your-api-key-here"
```

Alternatively, read from a file:

```shell
dev login --with-api-key < my_key.txt
```

The key must, at minimum, have write access to the Responses API. The legacy
`--api-key` flag exits with an error pointing at `--with-api-key`, so the key
never appears in shell history or process listings.

To switch from an API key to a ChatGPT plan, delete `~/.code/auth.json` (and the
legacy `~/.codex/auth.json` if it exists; on Windows both live under
`C:\Users\USERNAME\`) and run `dev login` again.

### Forcing a specific auth method

When both are available, choose which one wins:

```toml
# ~/.code/config.toml (a legacy ~/.codex/config.toml is also read)
preferred_auth_method = "apikey"   # or "chatgpt" (default)
```

Or ad-hoc: `dev --config preferred_auth_method="apikey"`.

With `"apikey"` and a key available, the login screen is skipped. With
`"chatgpt"`, ChatGPT auth wins if present and the API key is the fallback;
some account types require API-key mode regardless. `/status` in the TUI reports
which one the session is using, and the chat footer shows an “Auth: API key”
badge when a key is in play.

### Project `.env` safety

Dev does not read `OPENAI_API_KEY` or `AZURE_OPENAI_API_KEY` from a project's
local `.env` — many repos keep a key there for unrelated tooling, which would
silently spend against the key instead of your plan. There is no opt-in.

`~/.code/.env` (or `~/.codex/.env`) is loaded first and may hold a global
`OPENAI_API_KEY`, and a shell-exported one is honored.

## Connecting on a "headless" machine

`dev login` runs a server on `localhost:1455`. In a Docker container or over
`ssh`, your local browser cannot reach it, so use one of these.

### Copy credentials from a machine that has a browser

Complete `dev login` locally, then copy the resulting `$CODE_HOME/auth.json`
(defaults to `~/.code/auth.json`) across — it is not tied to a host. Into a
container:

```shell
# substitute MY_CONTAINER with the name or id of your Docker container:
CONTAINER_HOME=$(docker exec MY_CONTAINER printenv HOME)
docker exec MY_CONTAINER mkdir -p "$CONTAINER_HOME/.code"
docker cp auth.json MY_CONTAINER:"$CONTAINER_HOME/.code/auth.json"
```

Or onto a remote host:

```shell
ssh user@remote 'mkdir -p ~/.code && cat > ~/.code/auth.json' < ~/.code/auth.json
```

### Forward the login port

```bash
# From your local machine
ssh -L 1455:localhost:1455 <user>@<remote-host>
```

Then run `dev login` in that SSH session and open the printed
`http://localhost:1455/...` URL locally. The traffic is tunneled to the remote.
