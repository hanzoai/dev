(* ZAP Agent Example - OCaml Implementation
 *
 * Demonstrates using ZAP (Zero-copy Agent Protocol) tools from OCaml.
 * Uses hanzo-protocol types for unified approval/sandbox policies.
 *)

(* ============================================================================
 * hanzo-protocol Types (canonical source)
 * ============================================================================ *)

module ApprovalPolicy = struct
  type t =
    | Never           (* Never ask for approval *)
    | OnFailure       (* Only ask when operations fail *)
    | OnRequest       (* Model decides when to ask *)
    | UnlessTrusted   (* Ask unless operation is trusted (default) *)

  let to_string = function
    | Never -> "never"
    | OnFailure -> "on-failure"
    | OnRequest -> "on-request"
    | UnlessTrusted -> "unless-trusted"

  let default = UnlessTrusted
end

module SandboxPolicy = struct
  type t =
    | DangerFullAccess  (* No restrictions - use with caution *)
    | ReadOnly          (* Read-only filesystem access *)
    | WorkspaceWrite of {
        writable_roots: string list;
        network_access: bool;
      }

  let to_string = function
    | DangerFullAccess -> "danger-full-access"
    | ReadOnly -> "read-only"
    | WorkspaceWrite _ -> "workspace-write"

  let default = WorkspaceWrite { writable_roots = []; network_access = true }
end

(* ============================================================================
 * Executor Context
 * ============================================================================ *)

module ExecutorContext = struct
  type t = {
    cwd: string;
    env: (string * string) list;
    session_id: string;
    approval_policy: ApprovalPolicy.t;
    sandbox_policy: SandboxPolicy.t;
    timeout_ms: int;
  }

  let create
      ?(cwd = Sys.getcwd ())
      ?(env = [])
      ?(session_id = "ocaml-agent")
      ?(approval_policy = ApprovalPolicy.default)
      ?(sandbox_policy = SandboxPolicy.default)
      ?(timeout_ms = 30000)
      () =
    { cwd; env; session_id; approval_policy; sandbox_policy; timeout_ms }

  let default () = create ()
end

(* ============================================================================
 * Tool Result
 * ============================================================================ *)

module ToolResult = struct
  type metadata = (string * string) list

  type t = {
    id: string;
    content: string;  (* JSON string *)
    error: string option;
    metadata: metadata;
  }

  let success ~id ~content ?(metadata = []) () =
    { id; content; error = None; metadata }

  let failure ~id ~error ?(metadata = []) () =
    { id; content = ""; error = Some error; metadata }

  let is_success t = Option.is_none t.error
end

(* ============================================================================
 * ZAP Agent
 * ============================================================================ *)

module ZapAgent = struct
  type t = {
    name: string;
    version: string;
    context: ExecutorContext.t;
  }

  let create ~name ~version ~context =
    { name; version; context }

  (* Read file tool *)
  let read_file t ~path =
    let id = Printf.sprintf "read_file_%d" (Random.bits ()) in
    try
      let full_path =
        if Filename.is_relative path then
          Filename.concat t.context.cwd path
        else
          path
      in
      let ic = open_in full_path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      let json = Printf.sprintf {|{"content": %s, "size": %d}|}
        (Printf.sprintf "%S" content) n in
      ToolResult.success ~id ~content:json ()
    with
    | Sys_error msg ->
      ToolResult.failure ~id ~error:(Printf.sprintf "Failed to read file: %s" msg) ()

  (* List directory tool *)
  let list_dir t ~path =
    let id = Printf.sprintf "list_dir_%d" (Random.bits ()) in
    try
      let full_path =
        if Filename.is_relative path then
          Filename.concat t.context.cwd path
        else
          path
      in
      let entries = Sys.readdir full_path |> Array.to_list in
      let entries_json = entries
        |> List.map (fun e -> Printf.sprintf "%S" e)
        |> String.concat ", " in
      let json = Printf.sprintf {|{"entries": [%s], "count": %d}|}
        entries_json (List.length entries) in
      ToolResult.success ~id ~content:json ()
    with
    | Sys_error msg ->
      ToolResult.failure ~id ~error:(Printf.sprintf "Failed to list directory: %s" msg) ()

  (* Git status tool *)
  let git_status t =
    let id = Printf.sprintf "git_status_%d" (Random.bits ()) in
    try
      let ic = Unix.open_process_in
        (Printf.sprintf "cd %s && git status --porcelain -b 2>/dev/null"
          (Filename.quote t.context.cwd)) in
      let lines = ref [] in
      (try
        while true do
          lines := input_line ic :: !lines
        done
      with End_of_file -> ());
      let _ = Unix.close_process_in ic in
      let output = List.rev !lines in
      let branch = match output with
        | first :: _ when String.length first > 3 ->
          String.sub first 3 (String.length first - 3)
        | _ -> "unknown"
      in
      let staged = List.filter (fun l ->
        String.length l > 0 && l.[0] <> '?' && l.[0] <> ' '
      ) (List.tl output) in
      let unstaged = List.filter (fun l ->
        String.length l > 1 && l.[1] <> ' ' && l.[1] <> '?'
      ) (List.tl output) in
      let untracked = List.filter (fun l ->
        String.length l > 0 && l.[0] = '?'
      ) output in
      let json = Printf.sprintf
        {|{"branch": "%s", "staged_count": %d, "unstaged_count": %d, "untracked_count": %d}|}
        branch (List.length staged) (List.length unstaged) (List.length untracked) in
      ToolResult.success ~id ~content:json ()
    with
    | _ ->
      ToolResult.failure ~id ~error:"Failed to get git status" ()

  (* Execute command tool *)
  let exec t ~command =
    let id = Printf.sprintf "exec_%d" (Random.bits ()) in
    (* Check sandbox policy *)
    match t.context.sandbox_policy with
    | SandboxPolicy.ReadOnly ->
      ToolResult.failure ~id ~error:"Command execution not allowed in read-only mode" ()
    | _ ->
      try
        let full_command = Printf.sprintf "cd %s && %s 2>&1"
          (Filename.quote t.context.cwd) command in
        let ic = Unix.open_process_in full_command in
        let output = ref [] in
        (try
          while true do
            output := input_line ic :: !output
          done
        with End_of_file -> ());
        let status = Unix.close_process_in ic in
        let exit_code = match status with
          | Unix.WEXITED n -> n
          | Unix.WSIGNALED n -> 128 + n
          | Unix.WSTOPPED n -> 128 + n
        in
        let stdout = String.concat "\n" (List.rev !output) in
        let json = Printf.sprintf
          {|{"exit_code": %d, "stdout": %s, "stderr": ""}|}
          exit_code (Printf.sprintf "%S" stdout) in
        ToolResult.success ~id ~content:json ()
      with
      | Unix.Unix_error (err, _, _) ->
        ToolResult.failure ~id
          ~error:(Printf.sprintf "Command failed: %s" (Unix.error_message err)) ()

  (* List available tools *)
  let tools _ = [
    "read_file";
    "list_dir";
    "git_status";
    "exec";
  ]
end

(* ============================================================================
 * Main
 * ============================================================================ *)

let () =
  Random.self_init ();

  (* Create agent with workspace-write sandbox *)
  let context = ExecutorContext.create
    ~approval_policy:ApprovalPolicy.OnRequest
    ~sandbox_policy:(SandboxPolicy.WorkspaceWrite {
      writable_roots = [];
      network_access = true;
    })
    () in

  let agent = ZapAgent.create
    ~name:"ocaml-zap-agent"
    ~version:"0.1.0"
    ~context in

  Printf.printf "ZAP OCaml Agent Example\n";
  Printf.printf "=======================\n\n";

  Printf.printf "Agent: %s v%s\n" agent.name agent.version;
  Printf.printf "Approval Policy: %s\n"
    (ApprovalPolicy.to_string agent.context.approval_policy);
  Printf.printf "Sandbox Policy: %s\n"
    (SandboxPolicy.to_string agent.context.sandbox_policy);
  Printf.printf "Working Directory: %s\n\n" agent.context.cwd;

  Printf.printf "Available Tools:\n";
  List.iter (fun tool -> Printf.printf "  - %s\n" tool) (ZapAgent.tools agent);
  Printf.printf "\n";

  (* Demo: List directory *)
  Printf.printf "1. list_dir(\".\")\n";
  let result = ZapAgent.list_dir agent ~path:"." in
  (match result.error with
  | None -> Printf.printf "   Result: %s\n\n" result.content
  | Some e -> Printf.printf "   Error: %s\n\n" e);

  (* Demo: Read file *)
  Printf.printf "2. read_file(\"Cargo.toml\")\n";
  let result = ZapAgent.read_file agent ~path:"Cargo.toml" in
  (match result.error with
  | None ->
    let preview = String.sub result.content 0 (min 100 (String.length result.content)) in
    Printf.printf "   Result: %s...\n\n" preview
  | Some e -> Printf.printf "   Error: %s\n\n" e);

  (* Demo: Git status *)
  Printf.printf "3. git_status()\n";
  let result = ZapAgent.git_status agent in
  (match result.error with
  | None -> Printf.printf "   Result: %s\n\n" result.content
  | Some e -> Printf.printf "   Error: %s\n\n" e);

  (* Demo: Execute command *)
  Printf.printf "4. exec(\"echo Hello from ZAP!\")\n";
  let result = ZapAgent.exec agent ~command:"echo Hello from ZAP!" in
  (match result.error with
  | None -> Printf.printf "   Result: %s\n" result.content
  | Some e -> Printf.printf "   Error: %s\n" e);

  Printf.printf "\nZAP OCaml agent demo complete.\n"
