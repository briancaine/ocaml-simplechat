open Core

module type UI_type = sig

    type t

    val begin_connection : Conduit_lwt_unix.client -> t Lwt.t
    val begin_serving : Conduit_lwt_unix.server -> t Lwt.t
    val run :
      t ->
      (Conduit_lwt_unix.flow * Conduit_lwt_unix.ic * Conduit_lwt_unix.oc) ->
      unit Lwt.t

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

  UI.run ui_conn (flow, ic, oc)

let run_server ui_module_name mode =
  let module UI = (val (ui_of_name ui_module_name) : UI_type) in

  Conduit_lwt_unix.set_max_active 1;

  let%lwt ui_conn       = UI.begin_serving mode in
  let     ctx           = Conduit_lwt_unix.default_ctx in
  let     stop, wakener = Lwt.wait () in

  let on_exn exn =
    Lwt.wakeup wakener ();
    match exn with
    | Failure msg -> Printf.printf "Failure %s\n" msg
    | _           -> Printf.printf "Unknown failure\n"
  in

  Lwt.async_exception_hook := on_exn;

  let serve flow ic oc =
    let%lwt () =
        UI.run ui_conn (flow, ic, oc)
    in
    Lwt.wakeup wakener ();
    Lwt.return ()
  in

  Conduit_lwt_unix.serve ~ctx ~mode ~on_exn ~stop serve
