(** ZAP - Zero-copy Agent Protocol SDK for OCaml *)

let version = "0.6.0"
let max_message_size = 16 * 1024 * 1024 (* 16MB *)

(** Message types for ZAP wire protocol *)
module MessageType = struct
  type t =
    | Init
    | InitAck
    | ListTools
    | ListToolsResponse
    | CallTool
    | CallToolResponse
    | ListResources
    | ListResourcesResponse
    | ReadResource
    | ReadResourceResponse
    | ListPrompts
    | ListPromptsResponse
    | GetPrompt
    | GetPromptResponse
    | Ping
    | Pong
    | Error

  let to_byte = function
    | Init -> 0x01
    | InitAck -> 0x02
    | ListTools -> 0x10
    | ListToolsResponse -> 0x11
    | CallTool -> 0x12
    | CallToolResponse -> 0x13
    | ListResources -> 0x20
    | ListResourcesResponse -> 0x21
    | ReadResource -> 0x22
    | ReadResourceResponse -> 0x23
    | ListPrompts -> 0x30
    | ListPromptsResponse -> 0x31
    | GetPrompt -> 0x32
    | GetPromptResponse -> 0x33
    | Ping -> 0xF0
    | Pong -> 0xF1
    | Error -> 0xFF

  let of_byte = function
    | 0x01 -> Ok Init
    | 0x02 -> Ok InitAck
    | 0x10 -> Ok ListTools
    | 0x11 -> Ok ListToolsResponse
    | 0x12 -> Ok CallTool
    | 0x13 -> Ok CallToolResponse
    | 0x20 -> Ok ListResources
    | 0x21 -> Ok ListResourcesResponse
    | 0x22 -> Ok ReadResource
    | 0x23 -> Ok ReadResourceResponse
    | 0x30 -> Ok ListPrompts
    | 0x31 -> Ok ListPromptsResponse
    | 0x32 -> Ok GetPrompt
    | 0x33 -> Ok GetPromptResponse
    | 0xF0 -> Ok Ping
    | 0xF1 -> Ok Pong
    | 0xFF -> Ok Error
    | b -> Error (`Unknown_message_type b)
end

(** Approval policy for tool execution *)
module ApprovalPolicy = struct
  type t =
    | UnlessTrusted
    | OnFailure
    | OnRequest
    | Never
  [@@deriving yojson]

  let to_string = function
    | UnlessTrusted -> "unless-trusted"
    | OnFailure -> "on-failure"
    | OnRequest -> "on-request"
    | Never -> "never"
end

(** Sandbox mode for filesystem access *)
module SandboxMode = struct
  type t =
    | DangerFullAccess
    | ReadOnly
    | WorkspaceWrite
  [@@deriving yojson]

  let to_string = function
    | DangerFullAccess -> "danger-full-access"
    | ReadOnly -> "read-only"
    | WorkspaceWrite -> "workspace-write"
end

(** Sandbox policy configuration *)
module SandboxPolicy = struct
  type t = {
    mode: SandboxMode.t;
    writable_roots: string list;
    network_access: bool;
    allow_git_writes: bool;
  } [@@deriving yojson]

  let read_only = {
    mode = SandboxMode.ReadOnly;
    writable_roots = [];
    network_access = false;
    allow_git_writes = false;
  }

  let workspace_write ~roots = {
    mode = SandboxMode.WorkspaceWrite;
    writable_roots = roots;
    network_access = true;
    allow_git_writes = false;
  }
end

(** Tool definition *)
module Tool = struct
  type t = {
    name: string;
    description: string;
    input_schema: Yojson.Safe.t option;
  } [@@deriving yojson]
end

(** Tool call request *)
module ToolCall = struct
  type t = {
    id: string;
    name: string;
    args: Yojson.Safe.t;
  } [@@deriving yojson]
end

(** Tool execution result *)
module ToolResult = struct
  type t = {
    id: string;
    content: Yojson.Safe.t;
    error: string option;
  } [@@deriving yojson]
end

(** Server info from handshake *)
module ServerInfo = struct
  type capabilities = {
    tools: bool;
    resources: bool;
    prompts: bool;
  } [@@deriving yojson]

  type t = {
    name: string;
    version: string;
    capabilities: capabilities;
  } [@@deriving yojson]
end

(** Client info for handshake *)
module ClientInfo = struct
  type t = {
    name: string;
    version: string;
  } [@@deriving yojson]

  let default = { name = "zap-ocaml"; version }
end

(** Wire protocol helpers *)
module Wire = struct
  open Lwt.Syntax

  let read_u32_le ic =
    let buf = Bytes.create 4 in
    let* () = Lwt_io.read_into_exactly ic buf 0 4 in
    Lwt.return (Int32.to_int (Bytes.get_int32_le buf 0))

  let write_u32_le oc n =
    let buf = Bytes.create 4 in
    Bytes.set_int32_le buf 0 (Int32.of_int n);
    Lwt_io.write_from_exactly oc buf 0 4

  let send oc msg_type payload =
    let payload_bytes = match payload with
      | Some json -> Yojson.Safe.to_string json
      | None -> ""
    in
    let total_len = 1 + String.length payload_bytes in
    let* () = write_u32_le oc total_len in
    let* () = Lwt_io.write_char oc (Char.chr (MessageType.to_byte msg_type)) in
    let* () = if payload_bytes <> "" then Lwt_io.write oc payload_bytes else Lwt.return_unit in
    Lwt_io.flush oc

  let recv ic =
    let* total_len = read_u32_le ic in
    if total_len > max_message_size then
      Lwt.fail_with "message too large"
    else
      let* msg_byte = Lwt_io.read_char ic in
      let payload_len = total_len - 1 in
      let* payload = if payload_len > 0 then
        let buf = Bytes.create payload_len in
        let* () = Lwt_io.read_into_exactly ic buf 0 payload_len in
        Lwt.return (Some (Yojson.Safe.from_string (Bytes.to_string buf)))
      else
        Lwt.return None
      in
      match MessageType.of_byte (Char.code msg_byte) with
      | Ok msg_type -> Lwt.return (msg_type, payload)
      | Error (`Unknown_message_type b) ->
        Lwt.fail_with (Printf.sprintf "unknown message type: 0x%02x" b)
end

(** ZAP Client *)
module Client = struct
  type t = {
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
    mutable server_info: ServerInfo.t option;
    mutable request_id: int;
  }

  open Lwt.Syntax

  let connect host port =
    let* (ic, oc) = Lwt_io.open_connection (Unix.ADDR_INET (Unix.inet_addr_of_string host, port)) in
    let client = { ic; oc; server_info = None; request_id = 0 } in
    (* Handshake *)
    let* () = Wire.send oc MessageType.Init (Some (ClientInfo.to_yojson ClientInfo.default)) in
    let* (msg_type, payload) = Wire.recv ic in
    match msg_type, payload with
    | MessageType.InitAck, Some json ->
      (match ServerInfo.of_yojson json with
       | Ok info ->
         client.server_info <- Some info;
         Lwt.return (Ok client)
       | Error e -> Lwt.return (Error (`Protocol_error e)))
    | _ -> Lwt.return (Error (`Protocol_error "expected InitAck"))

  let connect_url url =
    (* Parse zap://host:port *)
    let url = if String.length url > 6 && String.sub url 0 6 = "zap://" then
      String.sub url 6 (String.length url - 6)
    else url in
    match String.split_on_char ':' url with
    | [host; port] -> connect host (int_of_string port)
    | _ -> Lwt.return (Error (`Invalid_url url))

  let server_info t = t.server_info

  let list_tools t =
    let* () = Wire.send t.oc MessageType.ListTools None in
    let* (msg_type, payload) = Wire.recv t.ic in
    match msg_type, payload with
    | MessageType.ListToolsResponse, Some json ->
      (match [%of_yojson: Tool.t list] json with
       | Ok tools -> Lwt.return (Ok tools)
       | Error e -> Lwt.return (Error (`Protocol_error e)))
    | _ -> Lwt.return (Error (`Protocol_error "expected ListToolsResponse"))

  let call_tool t name args =
    t.request_id <- t.request_id + 1;
    let call = ToolCall.{
      id = Printf.sprintf "req-%d" t.request_id;
      name;
      args;
    } in
    let* () = Wire.send t.oc MessageType.CallTool (Some (ToolCall.to_yojson call)) in
    let* (msg_type, payload) = Wire.recv t.ic in
    match msg_type, payload with
    | MessageType.CallToolResponse, Some json ->
      (match ToolResult.of_yojson json with
       | Ok result -> Lwt.return (Ok result)
       | Error e -> Lwt.return (Error (`Protocol_error e)))
    | _ -> Lwt.return (Error (`Protocol_error "expected CallToolResponse"))

  let ping t =
    let* () = Wire.send t.oc MessageType.Ping None in
    let* (msg_type, _) = Wire.recv t.ic in
    match msg_type with
    | MessageType.Pong -> Lwt.return (Ok ())
    | _ -> Lwt.return (Error (`Protocol_error "expected Pong"))

  let close t =
    let* () = Lwt_io.close t.ic in
    Lwt_io.close t.oc
end

(** ZAP Server *)
module Server = struct
  type tool_entry = {
    tool: Tool.t;
    handler: Yojson.Safe.t -> (Yojson.Safe.t, string) result Lwt.t;
  }

  type t = {
    name: string;
    version: string;
    mutable tools: (string, tool_entry) Hashtbl.t;
    mutable server: Lwt_io.server option;
  }

  open Lwt.Syntax

  let create ~name ~version () = {
    name;
    version;
    tools = Hashtbl.create 16;
    server = None;
  }

  let register_tool t ~name ~description ?input_schema handler =
    let entry = {
      tool = Tool.{ name; description; input_schema };
      handler;
    } in
    Hashtbl.replace t.tools name entry

  let handle_message t oc msg_type payload =
    match msg_type with
    | MessageType.Init ->
      let info = ServerInfo.{
        name = t.name;
        version = t.version;
        capabilities = { tools = true; resources = false; prompts = false };
      } in
      Wire.send oc MessageType.InitAck (Some (ServerInfo.to_yojson info))

    | MessageType.ListTools ->
      let tools = Hashtbl.fold (fun _ entry acc -> entry.tool :: acc) t.tools [] in
      Wire.send oc MessageType.ListToolsResponse (Some ([%to_yojson: Tool.t list] tools))

    | MessageType.CallTool ->
      (match payload with
       | Some json ->
         (match ToolCall.of_yojson json with
          | Ok call ->
            (match Hashtbl.find_opt t.tools call.name with
             | Some entry ->
               let* result = entry.handler call.args in
               let tool_result = match result with
                 | Ok content -> ToolResult.{ id = call.id; content; error = None }
                 | Error msg -> ToolResult.{ id = call.id; content = `Null; error = Some msg }
               in
               Wire.send oc MessageType.CallToolResponse (Some (ToolResult.to_yojson tool_result))
             | None ->
               let result = ToolResult.{ id = call.id; content = `Null; error = Some ("unknown tool: " ^ call.name) } in
               Wire.send oc MessageType.CallToolResponse (Some (ToolResult.to_yojson result)))
          | Error _ ->
            Wire.send oc MessageType.Error (Some (`Assoc [("message", `String "invalid tool call")])))
       | None ->
         Wire.send oc MessageType.Error (Some (`Assoc [("message", `String "missing payload")])))

    | MessageType.Ping ->
      Wire.send oc MessageType.Pong None

    | _ ->
      Wire.send oc MessageType.Error (Some (`Assoc [("message", `String "unsupported message type")]))

  let handle_connection t (ic, oc) =
    let rec loop () =
      Lwt.catch
        (fun () ->
           let* (msg_type, payload) = Wire.recv ic in
           let* () = handle_message t oc msg_type payload in
           loop ())
        (fun _ -> Lwt.return_unit)
    in
    loop ()

  let listen t port =
    let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    let* server = Lwt_io.establish_server_with_client_address addr (fun _ conn ->
      Lwt.async (fun () -> handle_connection t conn)
    ) in
    t.server <- Some server;
    Lwt.return_unit

  let close t =
    match t.server with
    | Some server ->
      t.server <- None;
      Lwt_io.shutdown_server server
    | None -> Lwt.return_unit
end
