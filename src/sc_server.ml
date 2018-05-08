open Core

(* loading UI implementations *)
let () = SimpleChatTextUI.init()

(* kicking off main function *)
let () =
  match Sys.argv with
  | [| _; ui_module_name; port |] ->
     let port    = Int.of_string port in
     let server  = `TCP (`Port port) in
     SimpleChat.run_server ui_module_name server
     |> Lwt_main.run
  | _ ->
     if Array.length Sys.argv = 0
     then failwith "Shouldn't reach here";
     Printf.printf
       "Bad args, usage:

%s ui_module port
"
       Sys.argv.(0)
