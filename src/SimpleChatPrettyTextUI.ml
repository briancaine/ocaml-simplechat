open Lwt.Infix

open Core

let rec chop_string ~count str =
  if String.length str <= count
  then [str]
  else
    (String.sub ~pos:0 ~len:count str) ::
    (String.sub ~pos:count ~len:(String.length str - count) str
     |> chop_string ~count)

let pad_spaces ~target str =
  str ^ String.make (target - String.length str) ' '

let format_time_delta delta =
  let ms = Time.Span.to_ms delta in
  if ms > 999.
  then "999+ms"
  else Printf.sprintf "%3.f ms" ms

open SimpleChat.Protocol

module Printable = struct

  type t = {
    style    : ANSITerminal.style list;
    contents : string;
  }

  let height { contents; _ } =
    String.split_lines contents
    |> List.length

  let draw_left_above { style; contents; } =
    let open ANSITerminal in
    save_cursor ();
    move_cursor 0 1;
    move_bol ();
    print_string style contents;
    restore_cursor()

  let list_height lst =
    List.map ~f:height lst
    |> List.fold ~init:0 ~f:(+)

  let draw_list_left_above lst =
    let open ANSITerminal in
    save_cursor ();
    move_cursor 0 (list_height lst);
    move_bol ();
    List.iter lst ~f:(fun { style; contents; } -> print_string style contents);
    restore_cursor()

  let blank size = {
    style    = [];
    contents = String.make size ' ';
  }

  let newline = {
    style = [];
    contents = "\n";
  }

  let right_padded_optional_field ~size ~style value =
    if value = None
    then [blank size]
    else [
      { contents = Option.value_exn value |> pad_spaces ~target:(size - 1);
        style; };
      { contents = " "; style = []; };
    ]

  let left_padded_optional_field ~size ~style value =
    if value = None
    then [blank size]
    else [
      { contents = " "; style = []; };
      { contents = Option.value_exn value |> pad_spaces ~target:(size - 1);
        style; };
    ]

end

module LogEvent = struct

  type severity =
    | Message
    | Info
    | Warning
    | Error [@@deriving sexp]

  type t = {
    severity   : severity;
    source     : Event.t;
    time       : Time.t;
    time_delta : Time.Span.t option;
    author     : Message.author option;
    contents   : Bytes.t;
    message_id : Message.id option;
  } [@@deriving sexp]

  let of_event ev = match ev with
    | Event.Message Message.{ id; author; time; contents; } ->
       {
         severity   = Message;
         source     = ev;
         time;
         time_delta = None;
         author     = Some author;
         contents;
         message_id = Some id;
       }
    | Event.MessageConfirmation _ -> failwith "shouldn't get here"
    | Event.ConnectionClosed ->
       {
         severity   = Info;
         source     = ev;
         time       = Time.now();
         time_delta = None;
         author     = None;
         contents   = "The remote user closed the chat.";
         message_id = None;
       }
    | Event.ConnectionWarning contents ->
       {
         severity   = Warning;
         source     = ev;
         time       = Time.now();
         time_delta = None;
         author     = None;
         contents;
         message_id = None;
       }
    | Event.ConnectionError contents ->
       {
         severity   = Error;
         source     = ev;
         time       = Time.now();
         time_delta = None;
         author     = None;
         contents;
         message_id = None;
       }

  let printables_of_log_event {
          severity;
          source;
          time;
          time_delta;
          author;
          contents;
          _
        } : Printable.t list =
    let (width, _)    = ANSITerminal.size() in
    let ts_width      = 5 + 1 in
    let author_width  = 4 + 1 in
    let arrow_width   = 1 + 1 in

    let confirm_width = 6 + 1 in

    let content_width =
      width - (ts_width + author_width + arrow_width + confirm_width) in

    let timestamp =
      Printable.right_padded_optional_field
        ~size:ts_width
        ~style:[]
        (Time.format time "%H:%M"
                     ~zone:(Lazy.force_val Time.Zone.local)
         |> Option.some) in
    let arrow =
      if author = None
      then
        [Printable.blank arrow_width]
      else
        Printable.right_padded_optional_field
          ~size:arrow_width
          ~style:[]
          (Some ">") in
    let author =
      Printable.right_padded_optional_field
        ~size:author_width
        ~style:[]
        Option.Monad_infix.(author >>| Message.string_of_author) in
    let time_delta =
      Printable.left_padded_optional_field
        ~size:confirm_width
        ~style:[]
        Option.Monad_infix.(time_delta >>| format_time_delta) in
    let lines = chop_string ~count:content_width contents
                |> List.map
                     ~f:(fun contents ->
                         [Printable.{ contents; style = []; }]) in

    let first, rest = List.hd_exn lines, List.tl_exn lines in

    (* whew, finally assembly: *)
    (timestamp @ author @ arrow @ first @ time_delta @ [Printable.newline]) ::
    (List.map ~f:(fun styled_line ->
                  [Printable.blank ts_width] @
                  [Printable.blank author_width] @
                  arrow @
                  styled_line @
                  [Printable.blank confirm_width] @
                  [Printable.newline])
              rest)

    |> List.concat

end

module Imp = struct

  type mode =
    | Server
    | Client [@@deriving sexp]

  type t = {
    mode : mode;
  } [@@deriving sexp]

  let begin_connection _ =
    Lwt.return { mode = Client; }

  let begin_serving _ =
    LogEvent.{
      severity = Message;
      source = Event.ConnectionClosed;
      time = Time.now();
      time_delta = Time.Span.of_ms 345. |> Option.some;
      author = Some Message.Us;
      contents = "abjkvlaweravjklwerajwervawerjklvaewrawerjklvjl;kawejklvjkawerjiovawijorvijoaweroijvawer";
      message_id = None;
    }
    |> LogEvent.printables_of_log_event
    |> Printable.draw_list_left_above;
    Lwt.return { mode = Server; }

  let run _ (flow, pull_stream, push) =

    let rec read_send_message () =
      Lwt_unix.sleep 1.0
    in

    let handle_event =
      let open SimpleChat.Protocol in
      function
      | ev -> Lwt.return ()
    in

    Lwt_stream.iter_s handle_event pull_stream
    <?>
    read_send_message ()

end

let init () =
  SimpleChat.add_ui_type "pretty_text" (module Imp : SimpleChat.UI_type)
