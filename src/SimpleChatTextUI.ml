open Lwt.Infix

open Core

module Imp = struct

  type t = unit

  let begin_connection client =
    Conduit_lwt_unix.sexp_of_client client
    |> Sexplib.Sexp.to_string_hum
    |> Lwt_io.printf "Connecting to: %s\n"

  let begin_serving server =
    Conduit_lwt_unix.sexp_of_server server
    |> Sexplib.Sexp.to_string_hum
    |> Lwt_io.printf "Listening on: %s\n"

  let run () (flow, pull_stream, push) =
    let last_msg_id = ref 0L in
    let%lwt () =
      Lwt_io.printf
        "Type messages to chat, enter sends. Just `/quit` exits.\n" in

    let handle_event =
      let open SimpleChat.Protocol in
      function
      | ev ->
         Event.sexp_of_t ev
         |> Sexplib.Sexp.to_string_hum ~indent:2
         |> Lwt_io.printf "Received event:\n%s\n"
    in

    let rec read_send_message () =
      let open SimpleChat.Protocol in
      let%lwt next_line = Lwt_io.(read_line stdin) in
      match String.strip next_line with
      | "/quit" ->
         push Event.ConnectionClosed
      | contents ->
         Int64.incr last_msg_id;
         let id     = !last_msg_id in
         let author = Message.Us in
         let time   = Time.now () in
         let msg    = Message.{ id; author; time; contents; } in
         let%lwt () = push (Event.Message msg) in
         read_send_message ()
    in

    Lwt_stream.iter_s handle_event pull_stream
    <?>
    read_send_message ()

end

let init () =
  SimpleChat.add_ui_type "text" (module Imp : SimpleChat.UI_type)
