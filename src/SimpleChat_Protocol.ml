open Core
open Sexplib.Std
open Bin_prot.Std

open Lwt.Infix

module Message = struct

  type id = int64 [@@deriving sexp, bin_io]

  type author =
    | Us
    | Them [@@deriving sexp, bin_io]

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

  (* clarify when ConnectionError and ConnectionWarning are local vs remote *)
  let remote_to_local = function
    | ConnectionWarning str ->
       ConnectionWarning (Printf.sprintf "Remote partner warned: %s" str)
    | ConnectionError str ->
       ConnectionError (Printf.sprintf "Remote partner error: %s" str)
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

end

let stream_of_conn flow ic oc =
  let awaiting_confirmation = ref (Int64.Table.create()) in
  let stream_open = ref true in

  let close_stream () =
    stream_open := false
  in

  let message_confirmed MessageConfirmation.{ id; _ } =
    match Core.Hashtbl.find_and_remove !awaiting_confirmation id with
    | None -> None
    | Some Message.{ time; _ } ->
       let time_delta = Time.diff (Time.now()) time in
       let conf       = MessageConfirmation.{ id; time_delta; } in
       Event.MessageConfirmation conf
       |> Option.some
  in

  let read_next_event () =
    let is_stream_open = Ref.(stream_open.contents) in
    (* ^ less confusing than !stream_open *)
    if not is_stream_open
    then
      Lwt.return None
    else (
      try%lwt
        let%lwt event = Event.read_t ic in
        match event with
        | Event.ConnectionClosed ->
           close_stream();
           event
           |> Lwt.return_some
        | Event.ConnectionWarning _ ->
           Event.remote_to_local event
           |> Lwt.return_some
        | Event.ConnectionError _ ->
           close_stream();
           Event.remote_to_local event
           |> Lwt.return_some
        | Event.MessageConfirmation conf ->
           (match message_confirmed conf with
            | Some event -> event
            | None       -> Event.unknown_confirmation_warning event)
           |> Lwt.return_some
        | _ -> event |> Lwt.return_some
      with
      | Lwt_io.Channel_closed desc ->
         close_stream();
         Event.ConnectionError (Printf.sprintf "Channel closed: %s" desc)
         |> Lwt.return_some
      | _ ->
         close_stream();
         Event.ConnectionError "Unknown exception reading from stream"
         |> Lwt.return_some
    )
  in
  Lwt_stream.from read_next_event
