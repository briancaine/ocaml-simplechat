open Core
open OUnit2

let buffer_size = 400 (* making this up *)

let test_serialize_event ev =
  test_case
    (fun ctxt ->
     let open SimpleChat.Protocol in
     (let     buffer = Lwt_bytes.create buffer_size in
      let     oc     = Lwt_io.(of_bytes ~mode:output buffer) in
      let%lwt ()     = Event.write_t oc ev in
      let     ic     = Lwt_io.(of_bytes ~mode:input buffer) in
      let%lwt nev    = Event.read_t ic in
      let     msg    =
        Printf.sprintf
          "Failed to serialize/unserialize Event\n  Expected: %s\n  Actual: %s\n"
          (Event.sexp_of_t ev |> Sexplib.Sexp.to_string_hum)
          (Event.sexp_of_t nev |> Sexplib.Sexp.to_string_hum)
      in
      assert_equal ev nev ~ctxt ~msg;
      Lwt.return ())
     |> Lwt_main.run)

let suite =
  let open SimpleChat in
  "SimpleChat" >:::
    let open Protocol in
    ["Protocol" >:::
       ["Event I/O" >:::(
           List.map
             [
               Event.Message Message.{
                 id       = 0L;
                 author   = Them;
                 time     = Time.epoch;
                 contents = "sup";
               };
               Event.MessageConfirmation MessageConfirmation.{
                 id         = 2L;
                 time_delta = Time.Span.of_ms 13.;
               };
               Event.ConnectionClosed;
               Event.ConnectionWarning "something's acting funny";
               Event.ConnectionError "something went awry";
             ]
             ~f:test_serialize_event
         )
       ]
    ]

let () = run_test_tt_main suite
