(** ZAP - Zero-copy Agent Protocol SDK for OCaml

    ZAP is a high-performance binary protocol for AI agent communication,
    1000x faster than MCP/JSON-RPC through zero-copy serialization. *)

val version : string
(** SDK version *)

val max_message_size : int
(** Maximum message size (16MB) *)

(** Message types for ZAP wire protocol *)
module MessageType : sig
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

  val to_byte : t -> int
  val of_byte : int -> (t, [> `Unknown_message_type of int ]) result
end

(** Approval policy for tool execution *)
module ApprovalPolicy : sig
  type t =
    | UnlessTrusted  (** Only auto-approve known-safe reads *)
    | OnFailure      (** Auto-approve, escalate on failure *)
    | OnRequest      (** Model decides (default) *)
    | Never          (** Never ask *)

  val to_string : t -> string
end

(** Sandbox mode for filesystem access *)
module SandboxMode : sig
  type t =
    | DangerFullAccess
    | ReadOnly
    | WorkspaceWrite

  val to_string : t -> string
end

(** Sandbox policy configuration *)
module SandboxPolicy : sig
  type t = {
    mode: SandboxMode.t;
    writable_roots: string list;
    network_access: bool;
    allow_git_writes: bool;
  }

  val read_only : t
  (** Read-only sandbox policy *)

  val workspace_write : roots:string list -> t
  (** Workspace write policy with specified writable roots *)
end

(** Tool definition *)
module Tool : sig
  type t = {
    name: string;
    description: string;
    input_schema: Yojson.Safe.t option;
  }
end

(** Tool call request *)
module ToolCall : sig
  type t = {
    id: string;
    name: string;
    args: Yojson.Safe.t;
  }
end

(** Tool execution result *)
module ToolResult : sig
  type t = {
    id: string;
    content: Yojson.Safe.t;
    error: string option;
  }
end

(** Server info from handshake *)
module ServerInfo : sig
  type capabilities = {
    tools: bool;
    resources: bool;
    prompts: bool;
  }

  type t = {
    name: string;
    version: string;
    capabilities: capabilities;
  }
end

(** Client info for handshake *)
module ClientInfo : sig
  type t = {
    name: string;
    version: string;
  }

  val default : t
end

(** ZAP Client *)
module Client : sig
  type t

  val connect : string -> int -> (t, [> `Protocol_error of string ]) result Lwt.t
  (** [connect host port] connects to a ZAP server *)

  val connect_url : string -> (t, [> `Protocol_error of string | `Invalid_url of string ]) result Lwt.t
  (** [connect_url "zap://host:port"] connects using a URL *)

  val server_info : t -> ServerInfo.t option
  (** Get server info from handshake *)

  val list_tools : t -> (Tool.t list, [> `Protocol_error of string ]) result Lwt.t
  (** List available tools *)

  val call_tool : t -> string -> Yojson.Safe.t -> (ToolResult.t, [> `Protocol_error of string ]) result Lwt.t
  (** [call_tool client name args] calls a tool *)

  val ping : t -> (unit, [> `Protocol_error of string ]) result Lwt.t
  (** Check connection *)

  val close : t -> unit Lwt.t
  (** Close the connection *)
end

(** ZAP Server *)
module Server : sig
  type t

  val create : name:string -> version:string -> unit -> t
  (** Create a new server *)

  val register_tool : t -> name:string -> description:string -> ?input_schema:Yojson.Safe.t ->
    (Yojson.Safe.t -> (Yojson.Safe.t, string) result Lwt.t) -> unit
  (** Register a tool handler *)

  val listen : t -> int -> unit Lwt.t
  (** Start listening on port *)

  val close : t -> unit Lwt.t
  (** Stop the server *)
end
