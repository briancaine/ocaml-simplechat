module Imp = struct

  type t = unit

  let begin_connection _ = failwith "debug finish"
  let begin_serving _ = failwith "debug finish"

  let run () (flow, ic, oc) =
    failwith "debug finish"

end

let init () =
  SimpleChat.add_ui_type "text" (module Imp : SimpleChat.UI_type)
