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

let debug_log_stream buf =
  Unix.with_file
    "/tmp/testlog.txt"
    ~mode:Unix.[O_CREAT; O_APPEND; O_WRONLY]
    ~f:(fun file_desc ->
        if String.length buf = Unix.single_write ~restart:true file_desc ~buf
        then ()
        else failwith "failed to write string")

module Printable = struct

  type t = {
    style    : ANSITerminal.style list;
    contents : string;
  }

  let height { contents; _ } =
    String.count contents ~f:((=) '\n')

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
    (let x, y = pos_cursor() in
     Printf.sprintf "debugk01 %d %d\n" x y
     |> debug_log_stream);
    save_cursor ();
    (let x, y = pos_cursor() in
     Printf.sprintf "debugk02 %d %d\n" x y
     |> debug_log_stream);
    move_bol ();
    (let x, y = pos_cursor() in
     Printf.sprintf "debugk03 %d %d:%d\n" x y (list_height lst)
     |> debug_log_stream);
    move_cursor 0 (- (list_height lst));
    List.iter lst ~f:(fun { style; contents; } ->
    (let x, y = pos_cursor() in
     Printf.sprintf "debugk04 %d %d\n" x y
     |> debug_log_stream);
                      erase Eol;
                      print_string style contents);
    (let x, y = pos_cursor() in
     Printf.sprintf "debugk05 %d %d\n" x y
     |> debug_log_stream);
    restore_cursor();
    (let x, y = pos_cursor() in
     Printf.sprintf "debugk06 %d %d\n" x y
     |> debug_log_stream)

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
    | Normal
    | Warning
    | Error [@@deriving sexp]

  type t = {
    severity   : severity;
    source     : Event.t option;
    time       : Time.t;
    time_delta : Time.Span.t option;
    author     : Message.author option;
    contents   : Bytes.t;
  } [@@deriving sexp]

  let of_event ev = match ev with
    | Event.Message Message.{ id; author; time; contents; } ->
       {
         severity   = Normal;
         source     = Some ev;
         time;
         time_delta = None;
         author     = Some author;
         contents;
       }
    | Event.MessageConfirmation _ -> failwith "shouldn't get here"
    | Event.ConnectionClosed ->
       {
         severity   = Normal;
         source     = Some ev;
         time       = Time.now();
         time_delta = None;
         author     = None;
         contents   = "The remote user closed the chat.";
       }
    | Event.ConnectionWarning contents ->
       {
         severity   = Warning;
         source     = Some ev;
         time       = Time.now();
         time_delta = None;
         author     = None;
         contents;
       }
    | Event.ConnectionError contents ->
       {
         severity   = Error;
         source     = Some ev;
         time       = Time.now();
         time_delta = None;
         author     = None;
         contents;
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
    mode               : mode;
    mutable log_events : LogEvent.t list;
    mutable line_count : int;
  } [@@deriving sexp]

  let rec redraw_state t =
    let printables = List.map ~f:LogEvent.printables_of_log_event t.log_events
                     |> List.concat in
    let line_count = Printable.list_height printables in
    let diff       = line_count - t.line_count in
    if diff > 0
    then (
      ANSITerminal.scroll diff;
      t.line_count <- line_count;
      ANSITerminal.move_cursor 0 1
    );
    Printable.draw_list_left_above printables

  let log_normal_string t contents =
    t.log_events <- t.log_events @ [
      LogEvent.{
        severity = Normal;
        time       = Time.now();
        time_delta = Time.Span.zero |> Option.some; (* debug *)
(*        time_delta = None; *)
        author     = None;
        source     = None;
        contents;
      }
    ]

  let log_event t ev =
    t.log_events <- t.log_events @ [LogEvent.of_event ev]

  let log_normal_string_redraw t contents =
    log_normal_string t contents;
    redraw_state t

  let log_event_redraw t ev =
    log_event t ev;
    redraw_state t

  let begin_connection client =
    let state = {
      mode       = Client;
      log_events = [];
      line_count = 0;
    } in
    Conduit_lwt_unix.sexp_of_client client
    |> Sexplib.Sexp.to_string_hum
    |> Printf.sprintf "Connecting to: %s"
    |> log_normal_string_redraw state;
    Lwt.return state

  let begin_serving server =
    let state = {
      mode       = Client;
      log_events = [];
      line_count = 0;
    } in
    Conduit_lwt_unix.sexp_of_server server
    |> Sexplib.Sexp.to_string_hum
    |> Printf.sprintf "Listening on: %s"
    |> log_normal_string_redraw state;
    Lwt.return state

  let run state (flow, pull_stream, push) =
    Conduit_lwt_unix.endp_of_flow flow
    |> Conduit.sexp_of_endp
    |> Sexplib.Sexp.to_string_hum
    |> Printf.sprintf "Conversing with: %s"
    |> log_normal_string_redraw state;

    let rec read_send_message () =
      let%lwt () = Lwt_unix.sleep 1.0 in
      read_send_message()
    in

    let handle_event =
      function
      | Event.MessageConfirmation _ ->
         log_normal_string_redraw state "debug finish: message confirmation";
         Lwt.return ()
      | ev ->
         log_event_redraw state ev;
         Lwt.return ()
    in

    Lwt_stream.iter_s handle_event pull_stream
    <?>
    read_send_message ()

end

let init () =
  SimpleChat.add_ui_type "pretty_text" (module Imp : SimpleChat.UI_type)
