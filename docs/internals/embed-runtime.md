# Dev embed runtime

The contract between a host (hanzo.ai/dev) and the Dev core, `apps/web/src/embed/runtime.ts`.

```text
The one interface between a host (hanzo.ai/dev) and the Dev core.

A host resolves a `DevRuntime` for the signed-in person and hands it to `DevProvider`.
Two kinds implement it:

  machine  the person's own computer, linked through Hanzo Link.
  cell     a Hanzo cloud sandbox leased through `/v1/dev` (HIP-1330) on api.hanzo.ai.

The client speaks ZAP to either, over WebTransport (HTTP/3): no WebSocket, no HTTP
session, no second auth. Hanzo IAM is the only identity: the client presents the
person's IAM access token when it opens the ZAP session.

The session is ONE bidirectional WebTransport stream. A message on it is a u32
little-endian length followed by one ZAP router envelope (`@zap-proto/zap` envelope.ts,
header flags = MSG_TYPE_ROUTER_BASE << 8):
  client -> runtime  request envelope  {method 0 "Frame", promiseID n, target 0,
                                         cap = the IAM bearer on the first message, else empty,
                                         payload = one frame of the environment protocol}
  runtime -> client  response envelope {status 200, promiseID, body = one frame}
A frame is the environment protocol's own message (packages/contracts, RpcSerialization
JSON today); the runtime unwraps and answers it, it does not translate it.

When `resolve` returns null the host keeps its previous Dev view: no runtime is not an
empty screen.
```
