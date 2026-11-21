// validate.mjs: decode every frame of a corpus against the Dev contracts.
//
//   node validate.mjs <dev checkout> <corpus.jsonl>
//
// A corpus line is {step, dir, f}: the form of testdata/chat.jsonl, which `go test ./frame -corpus` writes.
import fs from "node:fs";
const C = process.argv[2] + "/packages/contracts/";
const { WsRpcGroup } = await import(C + "src/rpc.ts");
import { createRequire } from "node:module";
const req = createRequire(C + "package.json");
const Schema = await import(req.resolve("effect/Schema"));
const RpcSchema = await import(req.resolve("effect/rpc/RpcSchema"));
const contracts = await import(C + "src/index.ts");
// The session's non-frame methods answer with the bodies of the Dev server's HTTP routes.
const calls = { descriptor: contracts.ExecutionEnvironmentDescriptor, session: contracts.AuthSessionState, shell: contracts.OrchestrationV2ShellHttpSnapshot };
const tags = new Map(); // requestId -> tag
let bad = 0, ok = 0;
const fail = (where, e) => { bad++; console.log("FAIL", where, String(e.message ?? e).slice(0, 600)); };
const dec = (s, v, where) => { try { Schema.decodeUnknownSync(Schema.toCodecJson(s))(v); ok++; } catch (e) { fail(where, e); } };
for (const line of fs.readFileSync(process.argv[3], "utf8").split("\n").filter(Boolean)) {
  const { step, dir, f } = JSON.parse(line);
  if (dir === "call") { dec(calls[step], f, `call ${step}`); continue; }
  if (f._tag === "Request") {
    tags.set(f.id, f.tag);
    const rpc = WsRpcGroup.requests.get(f.tag);
    if (!rpc) { fail(`${f.tag} request`, "unknown method"); continue; }
    dec(rpc.payloadSchema, f.payload, `${step} ${f.tag} payload`);
  } else if (dir === "in" && (f._tag === "Chunk" || f._tag === "Exit")) {
    const tag = tags.get(f.requestId); const rpc = WsRpcGroup.requests.get(tag);
    if (!rpc) { fail(`req ${f.requestId}`, "no request"); continue; }
    const st = RpcSchema.getStreamSchemas(rpc.successSchema);
    const { success, error } = st._tag === "Some" ? st.value : { success: rpc.successSchema, error: Schema.Never };
    if (f._tag === "Chunk") for (const v of f.values) dec(success, v, `${step} ${tag} chunk`);
    else if (f.exit._tag === "Success") dec(success, f.exit.value, `${step} ${tag} exit`);
    else for (const c of f.exit.cause) if (c._tag === "Fail") dec(Schema.Union([rpc.errorSchema, error]), c.error, `${step} ${tag} error`);
  }
}
console.log(`ok=${ok} bad=${bad}`);
process.exit(bad ? 1 : 0);
