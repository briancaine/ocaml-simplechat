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

  let run () (flow, ic, oc) =
    failwith "debug finish"

end

let init () =
  SimpleChat.add_ui_type "text" (module Imp : SimpleChat.UI_type)
