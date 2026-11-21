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
                                         cap = the IAM bearer on the first envelope that has one,
                                               else empty,
                                         payload = one frame of the environment protocol}
  runtime -> client  response envelope {status 200, promiseID, body = one frame}
Three more methods answer what a client asks before it opens the frame stream, as calls on
the same session (the Dev server answers the same questions over HTTP, byte for byte):
  1 descriptor  GET /.well-known/hanzo/environment  ExecutionEnvironmentDescriptor; public,
                answered before the stream holds a bearer
  2 session     GET /v1/auth/session                AuthSessionState
  3 shell       GET /v1/event/shell                 OrchestrationV2ShellHttpSnapshot, with its
                snapshotSequence; payload is the route's query as a JSON object
A call's promiseID has bit 30 set (`ZAP_CALL_ID_BASE`) and a frame's never does, and its
response echoes it, so a response says which it answers. The bearer authenticates the stream
from the first envelope that carries one; until then only `descriptor` is answered, and any
other method or frame is refused 401.

A frame is the environment protocol's own message (packages/contracts, RpcSerialization
JSON today); the runtime unwraps and answers it, it does not translate it.

When `resolve` returns null the host keeps its previous Dev view: no runtime is not an
empty screen.
```
