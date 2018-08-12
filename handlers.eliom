(**
Handlers for JSON RPC. For request and responce handling,
the actuall handler need to be registered in the
State module. It also sets up the echo hander for
keepalive massages
 *)

let (>>=) = Lwt.bind

let section = Lwt_log.Section.make "handlers"
let logger = Lwt_log.channel
               ~template:"$(name): $(section): $(loc-file): $(loc-line): $(message)"
               ~close_mode:`Keep ~channel:Lwt_io.stderr ()
let echo_reply j =
  let open Yojson.Basic.Util in
  let id_j = member "id" j in
  let param_j = member "params" j in
  let response = `Assoc [
                     "id", id_j;
                     "result", param_j;
                     "error", `Null;
                   ]
  in
  let str = (Yojson.Basic.to_string response) in
  match State.get_outch () with
  | None ->
     Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
       "Database out channel not sat"
  | Some outch ->
     let r = Lwt_io.write outch str in
     r >>= fun () ->
     Lwt.return ()

let handle_update j =
  let open Yojson.Basic.Util in
  let param_j = member "params" j in
  let id_j = member "id" j in
  let l = to_list param_j in
  if List.length l <> 2 then
    Lwt_log.info ~section ~logger ~location:(__FILE__, __LINE__, 0)
      ("Update method error in: " ^
         (Yojson.Basic.to_string j))
  else
    if id_j <> `Null then
      Lwt_log.info ~section ~logger ~location:(__FILE__, __LINE__, 0)
        ("Expected Null id for update in: " ^
         (Yojson.Basic.to_string j))
    else
      let mon_id = to_string (List.nth l 0) in
      if State.mem_response_handler mon_id then
        let f = State.find_response_handler mon_id in
        f j
      else
        Lwt_log.info ~section ~logger ~location:(__FILE__, __LINE__, 0)
          ("No monitor handler for monitor id: '" ^ mon_id ^ "' in " ^
             (Yojson.Basic.to_string j))



let handle_request j =
  let open Yojson.Basic.Util in
  let met_j = member "method" j in
  let method_str = to_string met_j in
  if State.mem_request_handler method_str then
    let f = State.find_request_handler method_str in
    f j
  else
    Lwt_log.info ~section ~logger ~location:(__FILE__, __LINE__, 0)
      ("Unknown request method: '" ^ method_str ^ "' in " ^
         (Yojson.Basic.to_string j))


let handle_response j =
  let open Yojson.Basic.Util in
  let id = to_string (member "id" j) in

  if State.mem_response_handler id then
    let f = State.find_response_handler id in
    let () = State.remove_response_handler id in
    f j
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      ("Unknown request id for response in: '" ^ id ^ "' in " ^
         (Yojson.Basic.to_string j))

let init_request_handlers () =
  let () = State.add_request_handler "echo" echo_reply in
  let () = State.add_request_handler "update" handle_update in
  ()
