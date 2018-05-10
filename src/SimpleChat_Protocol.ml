open Sexplib.Std
open Bin_prot.Std

open Lwt.Infix

open Core

module Message = struct

  type id = int64 [@@deriving sexp, bin_io]

  type author =
    | Us
    | Them [@@deriving sexp, bin_io]

  let string_of_author = function
    | Us   -> "Us"
    | Them -> "Them"

  type t = {
    id       : id;
    author   : author;
    time     : Time.t;
    contents : Bytes.t;
  } [@@deriving sexp, bin_io]

  let string_of_id = Int64.to_string

end

module MessageConfirmation = struct

  type t = {
    id         : Message.id;
    time_delta : Time.Span.t;
  } [@@deriving sexp, bin_io]

end

module Event = struct

  type t =
    | Message             of Message.t
    | MessageConfirmation of MessageConfirmation.t
    | ConnectionClosed
    | ConnectionWarning   of string
    | ConnectionError     of string [@@deriving sexp, bin_io]

  (* all this shuffling around of buffers would be nice to eliminate... *)

  let read_t ic =
    let%lwt size   = Lwt_io.BE.read_int64 ic >|= Int.of_int64_exn in
    let     buf    = String.make size '\000' in
    let%lwt ()     = Lwt_io.read_into_exactly ic buf 0 size in
    let     bp_buf = Bin_prot.Common.create_buf size in
    let     ()     = Bin_prot.Common.blit_string_buf buf bp_buf ~len:size in
    let     res    = bin_read_t ~pos_ref:(ref 0) bp_buf in
    Lwt.return res

  let write_t oc ev =
    Lwt_io.atomic (
        fun oc ->
        let     size   = bin_size_t ev in
        let     bp_buf = Bin_prot.Common.create_buf size in
        let     _      = bin_write_t bp_buf ~pos:0 ev in
        let     buf    = String.make size '\000' in
        let     ()     = Bin_prot.Common.blit_buf_string bp_buf buf ~len:size in
        let%lwt ()     = Int.to_int64 size |> Lwt_io.BE.write_int64 oc in
        let%lwt ()     = Lwt_io.write oc buf in
        Lwt.return ()
      ) oc

  (* clarify when ConnectionError and ConnectionWarning are local vs remote

     or update incoming messages (Us vs Them)
  *)
  let remote_to_local = function
    | ConnectionWarning str ->
       ConnectionWarning (Printf.sprintf "Remote partner warned: %s" str)
    | ConnectionError str ->
       ConnectionError (Printf.sprintf "Remote partner error: %s" str)
    | Message msg ->
       Message Message.{ msg with author = Them; }
    | other -> other

  (* if the other partner tries to confirm receipt of a message that we're not
     waiting on (either made up id or already confirmed id)

     we'll show a warning to the user
   *)
  let unknown_confirmation_warning = function
    | MessageConfirmation MessageConfirmation.{ id; _ } ->
       ConnectionWarning (
           Message.string_of_id id
           |> Printf.sprintf "Confirmation for unknown message: %s"
         )
    | _ -> failwith "shouldn't get here"

  (* message confirmation *)
  let confirmation_of_message = function
    | Message Message.{ id; _ } ->
       let time_delta = Time.Span.zero in
       MessageConfirmation MessageConfirmation.{ id; time_delta; }
    | _ -> failwith "shouldn't get here"

end

let stream_of_conn flow ic oc =
  let awaiting_confirmation = ref (Int64.Table.create()) in
  let stream_open = ref true in

  let close_stream () =
    stream_open := false
  in

  let message_confirmed MessageConfirmation.{ id; _ } =
    match Hashtbl.find_and_remove !awaiting_confirmation id with
    | None -> None
    | Some Message.{ time; _ } ->
       let time_delta = Time.diff (Time.now()) time in
       let conf       = MessageConfirmation.{ id; time_delta; } in
       Event.MessageConfirmation conf
       |> Option.some
  in

  let push event =
    (match event with
     | Event.Message data ->
        let key = Message.(data.id) in
        Hashtbl.add_exn !awaiting_confirmation ~key ~data
     | Event.ConnectionClosed ->
        stream_open := false
     | _ -> ());
    Event.write_t oc event
  in

  let handle_event event =
    match event with
    | Event.Message msg ->
       let%lwt () = Event.confirmation_of_message event
                    |> push in
       Event.remote_to_local event
       |> Lwt.return_some
    | Event.MessageConfirmation conf ->
       (match message_confirmed conf with
        | Some event -> event
        | None       -> Event.unknown_confirmation_warning event)
       |> Lwt.return_some
    | Event.ConnectionClosed ->
       close_stream ();
       Lwt.return_some event
    | Event.ConnectionWarning _ ->
       Event.remote_to_local event
       |> Lwt.return_some
    | Event.ConnectionError _ ->
       close_stream ();
       Event.remote_to_local event
       |> Lwt.return_some
  in

  let handle_read_exn exn =
    let is_stream_open = Ref.(stream_open.contents) in
    close_stream ();
    match exn with
    | End_of_file ->
       Event.ConnectionError "End_of_file"
       |> Lwt.return_some
    | Lwt_io.Channel_closed desc ->
       Event.ConnectionError (Printf.sprintf "Channel closed: %s" desc)
       |> Lwt.return_some
    | Unix.Unix_error (Unix.EBADF, _, _) when not is_stream_open ->
       Event.ConnectionClosed
       |> Lwt.return_some
    | _ ->
       Event.ConnectionError
         (Exn.to_string exn
          |> Printf.sprintf "Unknown error: %s")
       |> Lwt.return_some
  in

  let pull () =
    let is_stream_open = Ref.(stream_open.contents) in
    (* ^ less confusing than !stream_open *)
    if not is_stream_open
    then Lwt.return None
    else
      try%lwt
        Event.read_t ic >>= handle_event
      with
      | exn -> handle_read_exn exn
  in

  flow,
  Lwt_stream.from pull,
  push
