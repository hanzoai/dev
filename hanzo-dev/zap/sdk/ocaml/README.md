# hanzo-zap

Zero-copy Agent Protocol (ZAP) SDK for OCaml.

**1000x faster than MCP/JSON-RPC** through binary wire protocol with zero-copy serialization.

## Installation

```bash
opam install hanzo-zap
```

Or add to your dune-project:

```lisp
(depends (hanzo-zap (>= 0.6.0)))
```

## Quick Start

### Client

```ocaml
open Lwt.Syntax
open Hanzo_zap

let () = Lwt_main.run begin
  (* Connect to a ZAP server *)
  let* client = Client.connect_url "zap://localhost:9999" in
  match client with
  | Error _ -> Lwt.return_unit
  | Ok client ->
    (* List available tools *)
    let* tools = Client.list_tools client in
    (match tools with
     | Ok tools -> List.iter (fun t -> Printf.printf "Tool: %s\n" t.Tool.name) tools
     | Error _ -> ());

    (* Call a tool *)
    let* result = Client.call_tool client "read_file"
      (`Assoc [("path", `String "README.md")]) in
    (match result with
     | Ok r -> Printf.printf "Content: %s\n" (Yojson.Safe.to_string r.ToolResult.content)
     | Error _ -> ());

    Client.close client
end
```

### Server

```ocaml
open Lwt.Syntax
open Hanzo_zap

let () = Lwt_main.run begin
  let server = Server.create ~name:"my-tools" ~version:"1.0.0" () in

  (* Register a tool *)
  Server.register_tool server
    ~name:"greet"
    ~description:"Greet someone by name"
    (fun args ->
       match args with
       | `Assoc pairs ->
         (match List.assoc_opt "name" pairs with
          | Some (`String name) ->
            Lwt.return (Ok (`String (Printf.sprintf "Hello, %s!" name)))
          | _ ->
            Lwt.return (Error "missing name argument"))
       | _ -> Lwt.return (Error "invalid arguments"));

  (* Start server *)
  Printf.printf "ZAP server listening on port 9999\n%!";
  let* () = Server.listen server 9999 in
  fst (Lwt.wait ())  (* Wait forever *)
end
```

## Wire Protocol

ZAP uses a simple length-prefixed binary format:

```
+----------+----------+------------------+
| Length   | MsgType  | Payload          |
| (4 bytes)| (1 byte) | (variable)       |
| LE u32   |          | JSON             |
+----------+----------+------------------+
```

## API

### Client

- `Client.connect host port` - Connect to server
- `Client.connect_url url` - Connect using URL
- `Client.list_tools client` - List available tools
- `Client.call_tool client name args` - Call a tool
- `Client.ping client` - Check connection
- `Client.close client` - Close connection

### Server

- `Server.create ~name ~version ()` - Create server
- `Server.register_tool server ~name ~description handler` - Register tool
- `Server.listen server port` - Start listening
- `Server.close server` - Stop server

## Types

```ocaml
(* Approval policies *)
ApprovalPolicy.UnlessTrusted  (* Only auto-approve known-safe reads *)
ApprovalPolicy.OnFailure      (* Auto-approve, escalate on failure *)
ApprovalPolicy.OnRequest      (* Model decides (default) *)
ApprovalPolicy.Never          (* Never ask *)

(* Sandbox policies *)
SandboxPolicy.read_only
SandboxPolicy.workspace_write ~roots:["/home/user/project"]
```

## License

MIT - Hanzo AI Inc.
