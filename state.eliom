(**
This module has various hash tables to keep state for the application
- Request and response handlers
- Actve monitors
- Database meta information
*)

let response_tbl = Hashtbl.create 101

let add_response_handler (key : string)  (handler : Yojson.Basic.t -> unit Lwt.t) : unit =
  let ts = Unix.gettimeofday () in
  Hashtbl.add response_tbl key (ts, handler)

let remove_response_handler key =
  Hashtbl.remove response_tbl key

let mem_response_handler key =
  Hashtbl.mem response_tbl key

let find_response_handler key =
  let _, i = Hashtbl.find response_tbl key in
  i


let request_tbl = Hashtbl.create 101

let add_request_handler (key : string)  (handler : Yojson.Basic.t -> unit Lwt.t ) : unit  =
  let ts = Unix.gettimeofday () in
  Hashtbl.add request_tbl key (ts, handler)

let remove_request_handler key =
  Hashtbl.remove request_tbl key

let mem_request_handler key =
  Hashtbl.mem request_tbl key

let find_request_handler key =
  let _, i = Hashtbl.find request_tbl key in
  i

let monitor_tbl = Hashtbl.create 101

let add_monitor (key : string)  (db : string) (tbl : string) (col : string) : unit  =
  let ts = Unix.gettimeofday () in
  Hashtbl.add monitor_tbl key (ts, db, tbl, col)

let remove_monitor key =
  Hashtbl.remove monitor_tbl key

let mem_monitor key =
  Hashtbl.mem monitor_tbl key

let size_monitor key =
  Hashtbl.length monitor_tbl

let find_monitor key =
  let _, db, tbl, col = Hashtbl.find monitor_tbl key in
  db, tbl, col



let inch : Lwt_io.input Lwt_io.channel option ref  = ref None
let set_inch ch =
  inch := Some ch
let get_inch () =
  !inch

let outch : Lwt_io.output Lwt_io.channel option ref = ref None
let set_outch ch =
  outch := Some ch
let get_outch () =
  !outch




let meta_info = Hashtbl.create 101

let add_meta (key : string) data =
  let () = Hashtbl.remove meta_info key in
  let () = Hashtbl.add meta_info key data in
  ()

let get_meta key =
  if Hashtbl.mem meta_info key then
    let r = Hashtbl.find meta_info key  in
    r
  else
    "N/A"

let meta_info_cols : (string, string list) Hashtbl.t = Hashtbl.create 101

let add_meta_cols (key : string) data =
  let () = Hashtbl.remove meta_info_cols key in
  let () = Hashtbl.add meta_info_cols key data in
  ()

let get_meta_cols key =
  if Hashtbl.mem meta_info_cols key then
    let r = Hashtbl.find meta_info_cols key  in
    r
  else
    []

let meta_reference_cols : (string, string list) Hashtbl.t = Hashtbl.create 101

let add_reference (key : string) new_ref =
  if Hashtbl.mem meta_reference_cols key then
    let r = Hashtbl.find meta_reference_cols key  in
    let r = List.filter (fun e -> not (new_ref = e)) r in
    let () = Hashtbl.add meta_reference_cols key (new_ref::r) in
    ()
  else
    let () = Hashtbl.add meta_reference_cols key [new_ref] in
    ()

let get_reference (key : string) =
  if Hashtbl.mem meta_reference_cols key then
    let r = Hashtbl.find meta_reference_cols key  in
    r
  else
    []
