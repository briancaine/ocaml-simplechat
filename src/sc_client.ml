open Core

(* loading UI implementations *)
let () = SimpleChatTextUI.init()
let () = SimpleChatPrettyTextUI.init()

(* kicking off main function *)
let () =
  match Sys.argv with
  | [| _; ui_module_name; ip_addr; port |] ->
     let port    = Int.of_string port in
     let ip_addr = Ipaddr.of_string_exn ip_addr in
     let client  = `TCP (`IP ip_addr, `Port port) in
     SimpleChat.run_client ui_module_name client
     |> Lwt_main.run
  | _ ->
     if Array.length Sys.argv = 0
     then failwith "Shouldn't reach here";
     Printf.printf
       "Bad args, usage:

%s ui_module ip_addr port
"
       Sys.argv.(0)
