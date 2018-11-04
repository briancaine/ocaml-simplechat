Printf.printf "debug running tests\n";

open Core
open OUnit2

let test1 ctxt = assert_equal "X" (String.of_char_list ['X'])

let suite =
  "All" >:::
    ["test1" >:: test1]

let () = run_test_tt_main suite
