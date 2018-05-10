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

let lines_of_event (width, height) =
  let ts_width = 5 + 1 in
  let name_width = 4 + 1 in
  let arrow_width = 1 + 1 in

  let confirm_width = 6 + 1 in

  let content_width =
    width - (ts_width + name_width + arrow_width + confirm_width) in

  let open SimpleChat.Protocol in
  function
  | Event.Message Message.{ author; time; contents; _ } ->
     let time_str    = Time.format time "%H:%M"
                                   ~zone:(Lazy.force_val Time.Zone.local) in
     let author_str  = Message.string_of_author author in
     let confirm_str = "......" in
     let empty_prefix = String.make (ts_width + name_width) ' ' in
     let empty_suffix = String.make confirm_width ' ' in
     let parts       = chop_string ~count:content_width contents
                       |> List.map ~f:(pad_spaces ~target:content_width) in
     let first, rest = List.hd_exn parts, List.tl_exn parts in

     (time_str ^ " " ^ (pad_spaces ~target:name_width author_str) ^ "> " ^
        first ^ " " ^ confirm_str)
     ::
       List.map
         rest
         ~f:(fun section -> empty_prefix ^ "> " ^ section ^ empty_suffix)

  | _ -> failwith "debug finish"

(*
module Imp = struct

  (* ok so

     message types:

     AA:BB Us   >                                       ------|
     AA:BB Us   >                                       999+ms|
     AA:BB Them >                                             |
                >                                             |
                >                                             |
     AA:BB        Warning: asdfvawerv
     AA:BB        Connection lost, Error:
     AA:BB        Remote user ended chat
   *)


end

let init () =
  SimpleChat.add_ui_type "pretty_text" (module Imp : SimpleChat.UI_type)
 *)
