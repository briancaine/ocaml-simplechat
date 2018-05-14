open Lwt.Infix

open Core

open Cohttp
open Cohttp_lwt_unix

open SimpleChat

let estimate_static_dir () =
  match Sys.argv |> Array.to_list with
  | executable :: _ ->
     let _build_path_count = 4 in
     (* foo/bar/baz/.../_build/default/src/sc_client.exe
        need to drop the last four, then append "static"
     *)
     let parts = Filename.parts executable in
     let static_name = "webstatic" in
     if List.length parts >= (_build_path_count + 1)
     then
       let parts =
         List.rev parts
         |> fun lst -> List.drop lst _build_path_count
         |> List.rev in
       parts @ [static_name]
       |> Filename.of_parts
     else
       let dotdot_count = _build_path_count - (List.length parts - 2) - 1 in
       (Array.create ~len:dotdot_count ".." |> Array.to_list) @ [static_name]
       |> Filename.of_parts
  | [] -> failwith "Shouldn't reach here"

let base_static_dir = ref "."

let seconds_timeout = ref 10.
let sampling_duration = 1.

module UICommand = struct

  module SendMessage = struct

    type t = {
      id      : Protocol.Message.id;
      message : Bytes.t;
    }

    let of_yojson (json : Yojson.Basic.json) =
      let open Yojson.Basic.Util in
      Or_error.return {
          id      = member "id" json |> to_int |> Int64.of_int;
          message = member "message" json |> to_string |> Bytes.of_string;
        }

  end

  type t =
    | SendMessage of SendMessage.t
    | ConnectionClosed

  let of_yojson (json : Yojson.Basic.json) =
    let open Yojson.Basic.Util in
    match member "type" json |> to_string, member "value" json with
    | "send_message", value ->
       SendMessage.of_yojson value
       |> Or_error.map ~f:(fun item -> SendMessage item)
    | "connection_closed", `Null ->
       Or_error.return ConnectionClosed
    | _, _ ->
       Or_error.error_string "Invalid UI command"

  let list_of_yojson (json : Yojson.Basic.json) = match json with
    | `List items ->
       let res = List.map ~f:of_yojson items in
       if List.exists ~f:Or_error.is_error res
       then Or_error.error_string "Bad json"
       else List.map ~f:Or_error.ok_exn res |> Or_error.return
    | _           -> Or_error.error_string "Not a list"

end

module Imp = struct

  type t = {
    connect_string : string;
  }

  let begin_connection client =
    Lwt.return {
        connect_string =
          Conduit_lwt_unix.sexp_of_client client
          |> Sexplib.Sexp.to_string_hum
          |> Printf.sprintf "Connecting to: %s";
      }

  let begin_serving server =
    Lwt.return {
        connect_string =
          Conduit_lwt_unix.sexp_of_server server
          |> Sexplib.Sexp.to_string_hum
          |> Printf.sprintf "Listening on: %s";
      }

  let run { connect_string; } (flow, pull_stream, push) =

    let events      = ref [] in
    let stop, waken = Lwt.wait () in
    let last_update = ref None in

    let handle_event ev =
      events := !events @ [ev];
      Lwt.return ()
    in

    let handle_command =
      let open UICommand in
      function
      | ConnectionClosed ->
         Protocol.Event.ConnectionClosed |> push
      | SendMessage SendMessage.{ id; message; } ->
         let msg = Protocol.Message.{
             id;
             author = Us;
             time = Time.now();
             contents = message;
           } in
         Protocol.Event.Message msg
         |> push
    in

    let callback _conn req body =
      match Request.meth req, (Request.uri req |> Uri.path) with
      | `POST, "/api/update" ->
         last_update := Time.now() |> Option.some;
         Cohttp_lwt.Body.to_string body >|=
         Yojson.Basic.from_string >|=
         UICommand.list_of_yojson >>= fun commands ->
         if Or_error.is_error commands
         then
           Cohttp_lwt_unix.Server.respond_string
             ~status:`Bad_request ~body:"Malformed UICommands" ()
         else
           let     commands = Or_error.ok_exn commands in
           let%lwt ()   = Lwt_list.iter_s handle_command commands in
           let body     = Protocol.Event.to_yojson_list !events
                          |> Yojson.Basic.to_string in
           events := [];
           Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body()
      | `GET, path ->
         let uri = Request.uri req in
         let uri =
           if String.is_suffix ~suffix:"/" path
           then path ^ "index.html" |> Uri.with_path uri
           else uri in
         let fname = Server.resolve_local_file
                       ~docroot:(!base_static_dir) ~uri in
         Server.respond_file ~fname ()
      | meth, path ->
         let body = Printf.sprintf
                      "Method not allowed: %s %s"
                      (Code.string_of_method meth)
                      path in
         Server.respond_string
           ~status:`Method_not_allowed
           ~body
           ()
    in

    let rec attempt_serve_port port =
      try%lwt
        let%lwt () =
          Printf.sprintf "Attempting to start web UI on: http://localhost:%d/\n"
                         port
          |> Lwt_io.print in
        Server.create ~mode:(`TCP (`Port port)) ~stop
                      (Server.make ~callback ())
      with
      | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
         let%lwt () =
           Printf.sprintf "Failed to start web UI, trying next port.\n"
           |> Lwt_io.print in
         attempt_serve_port (port + 1)
      | exn -> Lwt.fail exn
    in

    let rec check_timeout () =
      if !last_update = None
      then
        let%lwt () = Lwt_unix.sleep sampling_duration in
        check_timeout ()
      else
        let now       = Time.now () in
        let last_time = !last_update |> fun x -> Option.value_exn x in
        if Time.diff now last_time |> Time.Span.to_sec < !seconds_timeout
        then
          let%lwt () = Lwt_unix.sleep sampling_duration in
          check_timeout ()
        else
          let%lwt () =
            Lwt_io.printf
              ("Web UI hasn't checked in in at least %f seconds, " ^^
                 "closing connection.\n")
              !seconds_timeout in
          let%lwt () =
            Protocol.Event.ConnectionError
              "User's UI stopped responding, closing connection."
            |> push in
          (try Lwt.wakeup waken () with | _ -> ());
          Lwt.return ()
    in

    let%lwt () =
      Lwt_stream.iter_s handle_event pull_stream
      <?>
      check_timeout ()
      <?>
      attempt_serve_port 8080
    in
    (try Lwt.wakeup waken () with | _ -> ());
    Lwt.return ()

end

let init () =
  base_static_dir := estimate_static_dir();
  SimpleChat.add_ui_type "web" (module Imp : SimpleChat.UI_type)
