open Core

module Protocol = struct
  include SimpleChat_Protocol
end


module type UI_type = sig

    type t

    val begin_connection : Conduit_lwt_unix.client -> t Lwt.t
    val begin_serving    : Conduit_lwt_unix.server -> t Lwt.t

    val run : t -> Protocol.stream -> unit Lwt.t

end

let add_ui_type, ui_of_name =
  let ui_modules = ref [] in
  let add (name : string) (ui_module : (module UI_type)) =
    ui_modules := (name, ui_module) :: !ui_modules
  in
  let get name = List.Assoc.find_exn !ui_modules ~equal:( = ) name in
  add, get

let run_client ui_module_name client =
  let module UI = (val (ui_of_name ui_module_name) : UI_type) in

  let%lwt ui_conn      = UI.begin_connection client in
  let     ctx          = Conduit_lwt_unix.default_ctx in
  let%lwt flow, ic, oc = Conduit_lwt_unix.connect ~ctx client in

  Protocol.stream_of_conn flow ic oc
  |> UI.run ui_conn

let run_server ui_module_name mode =
  let module UI = (val (ui_of_name ui_module_name) : UI_type) in

  (* todo:

     change this set_max_active to move that logic to serve

     because the web ui is based on conduit too:

   Conduit_lwt_unix.set_max_active 1;
 *)

  let%lwt ui_conn       = UI.begin_serving mode in
  let     ctx           = Conduit_lwt_unix.default_ctx in

  let on_exn exn =
    match exn with
    | Failure msg -> Printf.printf "Failure %s\n" msg
    | _           -> Printf.printf "Unknown failure\n"
  in

  Lwt.async_exception_hook := on_exn;

  let serve flow ic oc =
    Protocol.stream_of_conn flow ic oc
    |> UI.run ui_conn
  in

  Conduit_lwt_unix.serve ~ctx ~mode ~on_exn serve
