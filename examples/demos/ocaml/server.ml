(* OCaml + Switchback Demo
   Demonstrates Pattern Matching & Algebraic Data Types with Type-Safe State Machines

   This server showcases:
   - ADTs for modeling complex domain logic
   - Pattern matching for elegant state transitions
   - Type safety that prevents invalid states at compile time
   - Switchback for instant navigation between states
*)

open Unix
open Printf

(* ============================================================================
   DOMAIN MODEL - Algebraic Data Types
   ============================================================================ *)

(* Task workflow states - compiler prevents invalid states! *)
type workflow_state =
  | Draft
  | InReview of { reviewer: string; submitted_at: float }
  | Approved of { reviewer: string; approved_at: float }
  | Published of { reviewer: string; published_at: float; url: string }
  | Rejected of { reviewer: string; reason: string; rejected_at: float }

(* Task priority levels *)
type priority = Low | Medium | High | Critical

(* Main task type *)
type task = {
  id: int;
  title: string;
  description: string;
  priority: priority;
  state: workflow_state;
  created_at: float;
}

(* State transition results *)
type transition_result =
  | Success of task
  | InvalidTransition of string
  | NotFound

(* Global state - mutable reference to task list *)
let tasks : task list ref = ref []
let next_id = ref 1

(* ============================================================================
   STATE MACHINE LOGIC - Pattern Matching Excellence
   ============================================================================ *)

(* Perform state transition with validation *)
let transition_task (id: int) (action: string) (reviewer: string) (reason: string option) (url: string option) : transition_result =
  let task_opt = List.find_opt (fun t -> t.id = id) !tasks in
  match task_opt with
  | None -> NotFound
  | Some task ->
      let now = Unix.time () in
      match (task.state, action) with
      (* Draft → InReview *)
      | (Draft, "review") ->
          let new_state = InReview { reviewer; submitted_at = now } in
          let updated = { task with state = new_state } in
          tasks := List.map (fun t -> if t.id = id then updated else t) !tasks;
          Success updated

      (* InReview → Approved *)
      | (InReview _, "approve") ->
          let new_state = Approved { reviewer; approved_at = now } in
          let updated = { task with state = new_state } in
          tasks := List.map (fun t -> if t.id = id then updated else t) !tasks;
          Success updated

      (* InReview → Rejected *)
      | (InReview _, "reject") ->
          let reject_reason = Option.value reason ~default:"No reason provided" in
          let new_state = Rejected { reviewer; reason = reject_reason; rejected_at = now } in
          let updated = { task with state = new_state } in
          tasks := List.map (fun t -> if t.id = id then updated else t) !tasks;
          Success updated

      (* Approved → Published *)
      | (Approved _, "publish") ->
          let publish_url = Option.value url ~default:"/published" in
          let new_state = Published { reviewer; published_at = now; url = publish_url } in
          let updated = { task with state = new_state } in
          tasks := List.map (fun t -> if t.id = id then updated else t) !tasks;
          Success updated

      (* Rejected → Draft (resubmission) *)
      | (Rejected _, "draft") ->
          let new_state = Draft in
          let updated = { task with state = new_state } in
          tasks := List.map (fun t -> if t.id = id then updated else t) !tasks;
          Success updated

      (* Invalid transitions *)
      | (Published _, _) ->
          InvalidTransition "Cannot modify published tasks"
      | _ ->
          let current_state_str = match task.state with
            | Draft -> "Draft"
            | InReview _ -> "InReview"
            | Approved _ -> "Approved"
            | Published _ -> "Published"
            | Rejected _ -> "Rejected"
          in
          InvalidTransition (sprintf "Cannot perform action '%s' from state %s" action current_state_str)

(* ============================================================================
   JSON SERIALIZATION - Manual (no dependencies!)
   ============================================================================ *)

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let priority_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Critical -> "critical"

let state_to_json = function
  | Draft ->
      {|{"status":"draft","label":"Draft"}|}
  | InReview { reviewer; submitted_at } ->
      sprintf {|{"status":"in_review","label":"In Review","reviewer":"%s","submitted_at":%.0f}|}
        (escape_json_string reviewer) submitted_at
  | Approved { reviewer; approved_at } ->
      sprintf {|{"status":"approved","label":"Approved","reviewer":"%s","approved_at":%.0f}|}
        (escape_json_string reviewer) approved_at
  | Published { reviewer; published_at; url } ->
      sprintf {|{"status":"published","label":"Published","reviewer":"%s","published_at":%.0f,"url":"%s"}|}
        (escape_json_string reviewer) published_at (escape_json_string url)
  | Rejected { reviewer; reason; rejected_at } ->
      sprintf {|{"status":"rejected","label":"Rejected","reviewer":"%s","reason":"%s","rejected_at":%.0f}|}
        (escape_json_string reviewer) (escape_json_string reason) rejected_at

let task_to_json (t: task) : string =
  sprintf {|{"id":%d,"title":"%s","description":"%s","priority":"%s","state":%s,"created_at":%.0f}|}
    t.id
    (escape_json_string t.title)
    (escape_json_string t.description)
    (priority_to_string t.priority)
    (state_to_json t.state)
    t.created_at

let tasks_to_json_array tasks =
  "[\n" ^ String.concat ",\n" (List.map task_to_json tasks) ^ "\n]"

(* Get available actions for a state *)
let get_available_actions = function
  | Draft -> ["review"]
  | InReview _ -> ["approve"; "reject"]
  | Approved _ -> ["publish"]
  | Rejected _ -> ["draft"]
  | Published _ -> []

(* ============================================================================
   HTTP UTILITIES
   ============================================================================ *)

let send_response out_channel status content_type body =
  let headers = sprintf
    "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\n\r\n"
    status content_type (String.length body)
  in
  output_string out_channel headers;
  output_string out_channel body;
  flush out_channel

let send_json out_channel json =
  send_response out_channel "200 OK" "application/json" json

let send_html out_channel html =
  send_response out_channel "200 OK" "text/html; charset=utf-8" html

let send_404 out_channel =
  send_response out_channel "404 Not Found" "text/plain" "404 Not Found"

let send_js out_channel path =
  try
    let ic = open_in path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    send_response out_channel "200 OK" "application/javascript" content
  with _ ->
    send_404 out_channel

(* Parse HTTP request *)
let parse_request_line line =
  match String.split_on_char ' ' line with
  | method_ :: path :: _ -> (method_, path)
  | _ -> ("", "")

let has_switchback_header headers =
  List.exists (fun h ->
    let lower = String.lowercase_ascii h in
    String.starts_with ~prefix:"x-switchback:" lower
  ) headers

(* ============================================================================
   ROUTE HANDLERS - Switchback Integration
   ============================================================================ *)

let handle_main_page () =
  sprintf {|{"component":"Main","props":{"tasks":%s},"url":"/"}|}
    (tasks_to_json_array !tasks)

let handle_published_page url =
  (* Find the task with this published URL *)
  match List.find_opt (fun t ->
    match t.state with
    | Published { url = task_url; _ } -> task_url = url
    | _ -> false
  ) !tasks with
  | Some task ->
      sprintf {|{"component":"PublishedTask","props":{"task":%s},"url":"%s"}|}
        (task_to_json task) url
  | None ->
      (* URL not found - redirect to main page *)
      handle_main_page ()

let handle_task_detail_page id_str =
  try
    let id = int_of_string id_str in
    match List.find_opt (fun t -> t.id = id) !tasks with
    | Some task ->
        let actions = get_available_actions task.state in
        let actions_json = "[" ^ String.concat "," (List.map (sprintf {|"%s"|}) actions) ^ "]" in
        sprintf {|{"component":"TaskDetail","props":{"task":%s,"availableActions":%s},"url":"/task/%d"}|}
          (task_to_json task) actions_json id
    | None ->
        (* Redirect to main page on not found *)
        handle_main_page ()
  with _ ->
    (* Redirect to main page on invalid ID *)
    handle_main_page ()

(* API: Get all tasks *)
let handle_api_tasks () =
  sprintf {|{"tasks":%s}|} (tasks_to_json_array !tasks)

(* API: Create task *)
let handle_api_create_task body =
  (* Simple JSON parsing - extract fields *)
  let title_regex = Str.regexp {|"title"[  ]*:[  ]*"\([^"]*\)"|} in
  let desc_regex = Str.regexp {|"description"[  ]*:[  ]*"\([^"]*\)"|} in
  let priority_regex = Str.regexp {|"priority"[  ]*:[  ]*"\([^"]*\)"|} in

  let title =
    if Str.string_match title_regex body 0 then Str.matched_group 1 body
    else "Untitled Task"
  in
  let description =
    if Str.string_match desc_regex body 0 then Str.matched_group 1 body
    else ""
  in
  let priority_str =
    if Str.string_match priority_regex body 0 then Str.matched_group 1 body
    else "medium"
  in
  let priority = match priority_str with
    | "low" -> Low
    | "high" -> High
    | "critical" -> Critical
    | _ -> Medium
  in

  let new_task = {
    id = !next_id;
    title;
    description;
    priority;
    state = Draft;
    created_at = Unix.time ();
  } in

  incr next_id;
  tasks := new_task :: !tasks;

  sprintf {|{"task":%s}|} (task_to_json new_task)

(* API: Transition task state *)
let handle_api_transition id_str body =
  (* Extract action, reviewer, reason, url from JSON - use search instead of string_match *)
  let action_regex = Str.regexp {|"action"[ \t\n\r]*:[ \t\n\r]*"\([^"]*\)"|} in
  let reviewer_regex = Str.regexp {|"reviewer"[ \t\n\r]*:[ \t\n\r]*"\([^"]*\)"|} in
  let reason_regex = Str.regexp {|"reason"[ \t\n\r]*:[ \t\n\r]*"\([^"]*\)"|} in
  let url_regex = Str.regexp {|"url"[ \t\n\r]*:[ \t\n\r]*"\([^"]*\)"|} in

  try
    let id = int_of_string id_str in

    (* Use search_forward to find patterns anywhere in the string *)
    let action =
      try
        ignore (Str.search_forward action_regex body 0);
        Str.matched_group 1 body
      with Not_found -> ""
    in

    let reviewer =
      try
        ignore (Str.search_forward reviewer_regex body 0);
        Some (Str.matched_group 1 body)
      with Not_found -> None
    in

    let reason =
      try
        ignore (Str.search_forward reason_regex body 0);
        Some (Str.matched_group 1 body)
      with Not_found -> None
    in

    let url =
      try
        ignore (Str.search_forward url_regex body 0);
        Some (Str.matched_group 1 body)
      with Not_found -> None
    in

    let reviewer_val = Option.value reviewer ~default:"System" in

    (* Debug: print the parsed values *)
    printf "Transition request - ID: %d, Action: '%s', Reviewer: '%s'\n%!" id action reviewer_val;

    match transition_task id action reviewer_val reason url with
    | Success task ->
        sprintf {|{"success":true,"task":%s}|} (task_to_json task)
    | InvalidTransition msg ->
        sprintf {|{"success":false,"error":"%s"}|} (escape_json_string msg)
    | NotFound ->
        {|{"success":false,"error":"Task not found"}|}
  with e ->
    printf "Error in transition: %s\n%!" (Printexc.to_string e);
    sprintf {|{"success":false,"error":"Invalid request: %s"}|} (Printexc.to_string e)

(* ============================================================================
   REQUEST ROUTER
   ============================================================================ *)

let route_request method_ path headers body =
  let is_switchback = has_switchback_header headers in

  match (method_, path) with
  (* Page routes - return JSON for Switchback, HTML wrapper otherwise *)
  | ("GET", "/") ->
      let json = handle_main_page () in
      (json, is_switchback)

  | ("GET", path) when String.starts_with ~prefix:"/published/" path ->
      let json = handle_published_page path in
      (json, is_switchback)

  | ("GET", path) when String.starts_with ~prefix:"/task/" path ->
      let id_str = String.sub path 6 (String.length path - 6) in
      let json = handle_task_detail_page id_str in
      (json, is_switchback)

  (* POST to /task/:id - Handle state transitions with Switchback! *)
  | ("POST", path) when String.starts_with ~prefix:"/task/" path ->
      let id_str = String.sub path 6 (String.length path - 6) in
      (* Parse the POST body and perform transition *)
      (match handle_api_transition id_str body with
       | response when String.starts_with ~prefix:"{\"success\":true" response ->
           (* Success - return updated task detail page *)
           handle_task_detail_page id_str
       | error_response ->
           (* Error - return error in page format for Switchback onError *)
           error_response
      ), true

  (* API routes - always return JSON *)
  | ("GET", "/api/tasks") ->
      (handle_api_tasks (), true)

  | ("POST", "/api/tasks") ->
      (handle_api_create_task body, true)

  | ("POST", path) when String.starts_with ~prefix:"/api/tasks/" path && String.ends_with ~suffix:"/transition" path ->
      let id_str = String.sub path 11 (String.length path - 22) in  (* Extract ID between /api/tasks/ and /transition *)
      (handle_api_transition id_str body, true)

  (* Static files *)
  | ("GET", "/dist/app.js") ->
      ("SERVE_FILE:dist/app.js", true)

  | _ ->
      (* Redirect unknown routes to main page *)
      (handle_main_page (), is_switchback)

(* Generate HTML wrapper for initial page load *)
let html_wrapper page_json =
  sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OCaml Workflow - Switchback</title>
    <script>window.initialPage = %s;</script>
</head>
<body>
    <div data-swbk-app>
        <div style="padding: 2rem; text-align: center; background: #1a1a2e; color: #ff6b35;">
            <p>Loading Switchback app...</p>
        </div>
    </div>
    <script type="module" src="/dist/app.js"></script>
</body>
</html>|} page_json

(* ============================================================================
   HTTP SERVER
   ============================================================================ *)

let handle_client client_fd =
  try
    let in_channel = in_channel_of_descr client_fd in
    let out_channel = out_channel_of_descr client_fd in

    try
      (* Read request line *)
      let request_line = input_line in_channel in
      let (method_, path) = parse_request_line request_line in

      (* Read headers *)
      let rec read_headers acc =
        let line = input_line in_channel in
        if line = "\r" || line = "" then List.rev acc
        else read_headers (line :: acc)
      in
      let headers = read_headers [] in

      (* Read body if present *)
      let content_length_header = List.find_opt (fun h ->
        String.starts_with ~prefix:"Content-Length:" h
      ) headers in

      let body = match content_length_header with
        | Some h ->
            let len_str = String.trim (String.sub h 15 (String.length h - 15)) in
            let len = int_of_string len_str in
            really_input_string in_channel len
        | None -> ""
      in

      (* Route request *)
      let (response, is_json_only) = route_request method_ path headers body in

      (* Send response *)
      if String.starts_with ~prefix:"SERVE_FILE:" response then
        let file_path = String.sub response 11 (String.length response - 11) in
        send_js out_channel file_path
      else if is_json_only then
        send_json out_channel response
      else
        send_html out_channel (html_wrapper response);

      (* Close in the correct order *)
      flush out_channel;
      shutdown client_fd SHUTDOWN_ALL;
      close_in_noerr in_channel;
      close_out_noerr out_channel
    with
    | End_of_file ->
        shutdown client_fd SHUTDOWN_ALL;
        close_in_noerr in_channel;
        close_out_noerr out_channel
    | e ->
        printf "Error handling request: %s\n%!" (Printexc.to_string e);
        (try shutdown client_fd SHUTDOWN_ALL with _ -> ());
        close_in_noerr in_channel;
        close_out_noerr out_channel
  with
  | e ->
      printf "Error setting up client channels: %s\n%!" (Printexc.to_string e);
      (try shutdown client_fd SHUTDOWN_ALL with _ -> ());
      (try close client_fd with _ -> ())

(* Initialize sample tasks *)
let init_sample_tasks () =
  tasks := [
    {
      id = 1;
      title = "Implement user authentication";
      description = "Add login/logout functionality with JWT tokens";
      priority = High;
      state = Draft;
      created_at = Unix.time () -. 86400.0;
    };
    {
      id = 2;
      title = "Write API documentation";
      description = "Document all REST endpoints with examples";
      priority = Medium;
      state = InReview { reviewer = "Alice"; submitted_at = Unix.time () -. 3600.0 };
      created_at = Unix.time () -. 172800.0;
    };
    {
      id = 3;
      title = "Fix database migration";
      description = "Resolve schema conflicts in production";
      priority = Critical;
      state = Approved { reviewer = "Bob"; approved_at = Unix.time () -. 1800.0 };
      created_at = Unix.time () -. 7200.0;
    };
    {
      id = 4;
      title = "Update dependencies";
      description = "Upgrade all packages to latest stable versions";
      priority = Low;
      state = Published { reviewer = "Carol"; published_at = Unix.time () -. 900.0; url = "/blog/dependencies-updated" };
      created_at = Unix.time () -. 259200.0;
    };
  ];
  next_id := 5

(* Main server loop *)
let () =
  init_sample_tasks ();

  let port = 8000 in
  let addr = ADDR_INET (inet_addr_any, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in

  (* Setup graceful shutdown *)
  let running = ref true in
  let shutdown_handler _ =
    printf "\n🐫 Shutting down gracefully...\n%!";
    running := false;
    (* Close the listening socket to unblock accept() *)
    (try shutdown sock SHUTDOWN_ALL with _ -> ());
    (try close sock with _ -> ())
  in

  (* Install signal handlers *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle shutdown_handler);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle shutdown_handler);

  setsockopt sock SO_REUSEADDR true;
  bind sock addr;
  listen sock 10;

  printf "🐫 OCaml server listening on http://0.0.0.0:%d\n%!" port;
  printf "   Try the type-safe state machine at /tasks!\n%!";

  (* Accept loop with graceful shutdown *)
  while !running do
    try
      let (client_fd, _) = accept sock in
      handle_client client_fd
    with
    | Unix_error (EINTR, _, _) ->
        (* Interrupted by signal, check if we should continue *)
        if !running then () else raise Exit
    | Unix_error (EBADF, _, _) ->
        (* Socket was closed during shutdown *)
        running := false
    | e when not !running ->
        (* Error during shutdown, ignore *)
        printf "Error during shutdown: %s\n%!" (Printexc.to_string e)
  done;

  printf "🐫 Server stopped\n%!"
