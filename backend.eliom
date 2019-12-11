(**
Has the main logic for the ovsdb browser. It handles requests from
the frontend and forwards to the ovsdb server. Notifications from the server
is a pushed to the frontend as react events
 *)
let (>>=) = Lwt.bind

let section = Lwt_log.Section.make "main"
let logger =
  Lwt_log.channel
    ~template:"$(name): $(section): $(loc-file): $(loc-line): $(message)"
    ~close_mode:`Keep ~channel:Lwt_io.stderr ()


[%%shared

open Js_of_ocaml
type db_name = string
type tbl_name = string
type uuid_pos = int
type header_full = string list
type header = string list
type keys = string list
type links = string list
type elements_full = string list list
type elements = string list list

type notif_type =
  | Delete of string * string * string
  | Modify of (string * string )
  | Insert of db_name * tbl_name * uuid_pos * header_full
              * header * keys * links
              * elements_full * elements
  | Statistic of (string * string)
]

module Notif =
  Eliom_notif.Make_Simple (struct
    type identity = int64
    type key = string
    type notification = notif_type list
    let get_identity () = Lwt.return (Int64.of_int 2)
  end)


let update_counters = Hashtbl.create 101

let add_notification db tbl =
   let  c =
    if Hashtbl.mem update_counters db then
      Hashtbl.find update_counters db
    else
      0
  in
  Hashtbl.replace update_counters db (c +1);
  let key = db ^ "." ^ tbl in
   let  c =
    if Hashtbl.mem update_counters key then
      Hashtbl.find update_counters key
    else
      0
  in
  Hashtbl.replace update_counters key (c + 1)

let get_notifications () =
  Hashtbl.fold (fun k v acc -> (k, v)::acc) update_counters []

let clear_notification () =
  let keys = Hashtbl.fold (fun k _ acc -> k::acc) update_counters [] in
  List.iter (fun key -> Hashtbl.replace update_counters key 0) keys

let key_ref = ref "foo"

let%client row_table : (string,  (int * bool * Html_types.text Eliom_content.Html.D.wrap list *
                                    string Eliom_content.Html.D.wrap Eliom_content.Html.D.wrap
                                      Eliom_content.Html.D.wrap Eliom_content.Html.D.wrap
                                      Eliom_content.Html.D.wrap list * string list * string list *
                                      string * string)
                                   ReactiveData.RList.handle
                       ) Hashtbl.t = Hashtbl.create 101

let%client add_row_table key handle =
  Hashtbl.remove row_table key;
  Hashtbl.add row_table key handle

let%client get_row_table key =
  Hashtbl.find row_table key

[%%shared
let remove_uuid l r =
  List.filter (fun e -> if e = r then false else true) l


let dont_show_version = ref true

let get_uuid_pos l e =
  let rec loop l pos =
    match l with
    | [] -> -1
    | hd::tl ->
       if hd = e then
         pos
       else
         loop tl (pos +1)
  in
  loop (List.rev l) 0

let tbl_row_index = Hashtbl.create 101

let add_row_index (key :string) (value : string) =
  let l =
    if Hashtbl.mem tbl_row_index key then
      let l = Hashtbl.find tbl_row_index key in
      Hashtbl.remove tbl_row_index key;
      l
    else
      []
  in
  Hashtbl.add tbl_row_index key (value::l)


let get_row_index key value =
  if Hashtbl.mem tbl_row_index key then
    let l = Hashtbl.find tbl_row_index key in
    get_uuid_pos l value
  else
    -1


let remove_row_index key value =
  if Hashtbl.mem tbl_row_index key then
    let l = Hashtbl.find tbl_row_index key in
    let ul = remove_uuid l value in
    Hashtbl.replace tbl_row_index key ul
  else
    ()

let drop_row_index key =
  if (Hashtbl.mem tbl_row_index key) then
    Hashtbl.remove tbl_row_index key
  else
    ()
]


let notify_cashe = ref []

let send_notification() =
  let no = List.rev !notify_cashe in
  notify_cashe := [];
  begin
    if no != [] then
      begin
        Notif.notify (!key_ref :  Notif.key) no
      end
    else
      ()
  end

let cash_notify e =
  let () = notify_cashe := e::!notify_cashe in
  if (List.length !notify_cashe > 100) then
    send_notification ()
  else
    ()

let send_notification_th () =
  let rec loop () =
    Lwt_unix.sleep 0.5 >>= fun () ->
    let () = send_notification () in
    loop ()
  in
  loop ()


let%server notify k n =
  cash_notify n

let bytes_read = ref 0

let notify_th () =
  let interval = 10.0 in
  let () = clear_notification () in
  let rec loop () =
    let%lwt () = Lwt_unix.sleep interval in
    let notifs = get_notifications () in
    let () = clear_notification () in
    let () = Printf.printf "Notifications:\n" in
    let speed = Printf.sprintf " %.2f Kbps" (((float_of_int !bytes_read) /. interval /. 1024.0) *. 8.0) in
    let () =  notify "foo" (Statistic ("_json_rpc_speed", speed)) in
    bytes_read := 0;
    let () = Printf.printf "%s\n" speed in
    let () = List.iter (fun (k, v) ->
                 let nps = (float_of_int v) /. interval in
                 let nstr = Printf.sprintf " %.2f" nps in
                 let () = if v > 0 then
                            let () =  notify "foo"
                                        (Statistic (k, (nstr ^ " notifications/second"))) in
                            ()
                          else
                            let () =  notify "foo"
                                        (Statistic (k, (""))) in
                            ()
                 in
                 let () = Printf.printf " %s %s\n" k nstr in
                 ()
               ) notifs in
    let () = Printf.printf "\n" in
    flush stdout;
    loop ()
  in
  loop ()


let%server listen () =
  Notif.init () >>= fun () ->
  Lwt.async (send_notification_th );
  Notif.listen !key_ref;
  let e : (string * notif_type list) Eliom_react.Down.t = Notif.client_ev () in
  ignore [%client
             ((React.E.map (fun (_, l) ->
                   List.iter (fun (value) ->
                       match value with
                       | Statistic (key, value) ->
                          begin
                            try
                              let el = Dom_html.getElementById key in
                              el##.innerHTML := Js.string value;
                            with Not_found ->
                              Printf.printf "Element not found"
                          end;
                       | Modify (key, value) ->
                          begin
                            try
                              let el = Dom_html.getElementById key in
                              el##.innerHTML := Js.string (value);
                              el##.title := Js.string value;
                              ()
                            with Not_found ->
                              Printf.printf "Element not found"
                          end;
                       | Delete (db_name, tbl, uuid) ->
                          let key = db_name ^ "." ^ tbl  in
                          let ha2 = get_row_table key in
                          let dbn = db_name ^ "." ^ tbl in
                          let index = get_row_index dbn uuid in
                          Eliom_shared.ReactiveData.RList.remove index ha2;
                          let () = remove_row_index dbn uuid in
                          ()
                       | Insert (db_name, tbl, uuid_pos, header_full,
                                 header, keys, links, elements_full, elements) ->
                          let key = db_name ^ "." ^ tbl  in
                          let ha2 = get_row_table key in
                          Eliom_shared.ReactiveData.RList.snoc
                            (uuid_pos, false, (List.hd elements_full), (List.hd elements),
                             keys, links, db_name, tbl) ha2;
                     ) l;
                 ) ~%e)
              : unit React.E.t)
    ];
  Lwt.return ()


let first_click = ref 0.0

let do_connect host port =
  let chans = Helpers.connect_to_db host port in
  chans >>= fun (outch, inch) ->
  let s = Printf.sprintf "Connected to db at %s:%d\n" host port in
  Lwt_log.debug ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->
  let () = State.set_outch outch in
  let () = State.set_inch inch in
  Lwt.return ()

let receiver_th host port () =
  let rec outer_loop () =
    let%lwt () = do_connect host port in
    match State.get_inch() with
    | None ->
       Lwt_log.error ~section ~logger ~location:(__FILE__, __LINE__, 0)
         "No in channel to database"
    | Some inch ->
       let rec loop () =
         let st =
           Helpers.read_next_json_object inch
         in
         st >>= fun s1 ->
         try
           let () = bytes_read := !bytes_read + (String.length s1) in
           if (String.length s1 == 0) then
             outer_loop ()
           else
             let j = (Yojson.Basic.from_string s1) in
             let open Yojson.Basic.Util in
             let req = member "method" j in
             let response = member "result" j in
             begin
               if req <> `Null then
                 let r = Handlers.handle_request  j in
                 r >>= fun () ->
                 loop ()
               else if response <> `Null then
                 let r = Handlers.handle_response j in
                 r >>= fun () ->
                 loop ()
               else
                 let s = Printf.sprintf "Unhandled json %s" (Yojson.Basic.pretty_to_string  j) in
                 Lwt_log.debug ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->
                 let%lwt () = Lwt_unix.sleep 1.0 in
                 loop ()
             end
         with  Yojson.Json_error es ->
           Lwt_log.error ~section ~logger ~location:(__FILE__, __LINE__, 0)
             ("Failed to parse: " ^ s1 ) >>= fun () ->
           Lwt_log.error ~section ~logger ~location:(__FILE__, __LINE__, 0)
             es  >>= fun () ->
           loop ()
       in
       loop ()
  in
  outer_loop ()


let process_notification db_name json =
  let tbl_l = Yojson.Basic.Util.keys json in
  List.iter (fun tbl ->
      let vals = Yojson.Basic.Util.member tbl json in
      let l = Yojson.Basic.Util.keys vals in
      let () = List.iter (fun assco ->
                   let () = add_notification db_name tbl in
                   let n = Yojson.Basic.Util.member assco vals in
                   let nv = Yojson.Basic.Util.member "new" n in
                   match nv with
                   | `Null ->
                      ()
                   | `List _ | `Bool _|`Assoc _|`Float _|`String _|`Int _ ->
                      let keys = Yojson.Basic.Util.keys nv in
                      List.iter (fun k ->
                          let v = Yojson.Basic.pretty_to_string 
                                    (Yojson.Basic.Util.member k nv) in
                          let v =
                            if String.length v > 0 then
                              begin
                                let c = String.get v 0 in
                                if c != '"' then
                                  v
                                else
                                  String.sub v 1 ((String.length v) - 2)
                              end
                            else
                              v
                          in
                          let key = db_name ^ "." ^ assco ^ "." ^ k in
                          let () = notify "foo" (Modify (key, v)) in
                          ()
                        ) keys
                 ) l
      in
      ()
    ) tbl_l

let has_old n =
  let nv = Yojson.Basic.Util.member "old" n in
  match nv with
  | `Null -> false
  | `List _ | `Bool _|`Assoc _|`Float _|`String _|`Int _ -> true

let has_new n =
  let nv = Yojson.Basic.Util.member "new" n in
  match nv with
  | `Null -> false
  | `List _ | `Bool _|`Assoc _|`Float _|`String _|`Int _ -> true


let process_notification_insert_delete db_name json =
  let tbl_l = Yojson.Basic.Util.keys json in
  List.iter (fun tbl ->
      let vals = Yojson.Basic.Util.member tbl json in
      let l = Yojson.Basic.Util.keys vals in
      let () = List.iter (fun assco ->
                   let () = add_notification db_name tbl in
                   let n = Yojson.Basic.Util.member assco vals in
                   let ho = has_old n in
                   if ho then
                     let () = notify "foo" (Delete (db_name, tbl, assco)) in
                     ()
                   else
                     let nv = Yojson.Basic.Util.member "new" n in
                     match nv with
                     | `Null ->
                        begin
                          let s = Printf.sprintf "initial/modify notifcation has no 'new' value: %s" (Yojson.Basic.pretty_to_string  json) in
                          Printf.printf "ERROR: %s\n" s;
                          ()
                        end
                     | `List _ | `Bool _|`Assoc _|`Float _|`String _|`Int _ ->
                        let ass_list = Yojson.Basic.Util.to_assoc nv in
                        let with_uuid = ("_uuid", `List [`String "uuid"; `String assco])::ass_list in
                        let all_ass : Yojson.Basic.json = `Assoc with_uuid in
                        let dbn = "\"" ^ db_name ^ "\"" in
                        let only_keys, keys, keys_short, values, values_short =
                          Helpers.process_row_l dbn tbl [all_ass] in
                        let uuid_pos = Helpers.list_pos only_keys "_uuid"  in
                        let links = Helpers.create_link_list only_keys ("\"" ^ db_name ^ "\"." ^ tbl ^ ".") in

                        let () = notify "foo" (Insert (db_name, tbl, uuid_pos, keys, keys_short, only_keys, links, values, values_short)) in

                        ()
                 ) l
      in
      ()
    ) tbl_l

let process_notification_all db_name json =
  let tbl_l = Yojson.Basic.Util.keys json in
  List.iter (fun tbl ->
      let vals = Yojson.Basic.Util.member tbl json in
      let l = Yojson.Basic.Util.keys vals in
      let () = List.iter (fun assco ->
                   let () = add_notification db_name tbl in
                   let n = Yojson.Basic.Util.member assco vals in
                   let ho = has_old n in
                   let hn = has_new n in
                   if ho && hn then
                     (* This is an update *)
                     let nv = Yojson.Basic.Util.member "new" n in
                     let ov = Yojson.Basic.Util.member "old" n in
                     let keys = Yojson.Basic.Util.keys ov in
                     List.iter (fun k ->
                         let v = if k  = "_version" && !dont_show_version then
                                   "..."
                                 else
                                   let v = Yojson.Basic.pretty_to_string (Yojson.Basic.Util.member k nv) in
                                   if String.length v > 0 then
                                     begin
                                       let c = String.get v 0 in
                                       if c != '"' then
                                         v
                                       else
                                         String.sub v 1 ((String.length v) - 2)
                                     end
                                   else
                                     v
                         in
                         let key = db_name ^ "." ^ assco ^ "." ^ k in
                         let () = notify "foo" (Modify (key, v)) in
                         ()
                       ) keys;
                     ()
                   else if ho then
                     (* This is a delete *)
                     let () = notify "foo" (Delete (db_name, tbl, assco)) in
                     ()
                   else
                     let nv = Yojson.Basic.Util.member "new" n in
                     match nv with
                     | `Null ->
                        begin
                          let s = Printf.sprintf "initial/modify notifcation has no 'new' value: %s" (Yojson.Basic.pretty_to_string  json) in
                          Printf.printf "ERROR: %s\n" s;
                          ()
                        end
                     | `List _ | `Bool _|`Assoc _|`Float _|`String _|`Int _ ->
                        let ass_list = Yojson.Basic.Util.to_assoc nv in
                        let with_uuid = ("_uuid", `List [`String "uuid"; `String assco])::ass_list in
                        let all_ass : Yojson.Basic.json = `Assoc with_uuid in
                        let dbn = "\"" ^ db_name ^ "\"" in
                        let only_keys, keys, keys_short, values, values_short =
                          Helpers.process_row_l dbn tbl [all_ass] in
                        let uuid_pos = Helpers.list_pos only_keys "_uuid"  in
                        let links = Helpers.create_link_list only_keys ("\"" ^ db_name ^ "\"." ^ tbl ^ ".") in
                        let () = notify "foo" (Insert (db_name, tbl, uuid_pos, keys, keys_short, only_keys, links, values, values_short)) in
                        ()
                 ) l
      in
      ()
    ) tbl_l


let mon1 db_name j =
  let open Yojson.Basic.Util in
  let param_j = member "params" j in
  let l = to_list param_j in
  let updated_tbls = List.nth l 1 in
  let () = process_notification db_name updated_tbls in
  Lwt.return ()


let mon_init db_name j =
  let open Yojson.Basic.Util in
  let param_j = member "result" j in
  let () = process_notification db_name param_j in
  Lwt.return ()

let mon_init_table db_name j =
  let open Yojson.Basic.Util in
  Lwt.return ()


let mon_remove j =
  Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
    ("Monitor init: " ^ (Yojson.Basic.pretty_to_string  j))


let mon2 db_name j =
  let open Yojson.Basic.Util in
  let param_j = member "params" j in
  let l = to_list param_j in
  let updated_tbls = List.nth l 1 in
  let () = process_notification_insert_delete db_name updated_tbls in
  Lwt.return ()


let mon3 db_name j =
  let open Yojson.Basic.Util in
  let param_j = member "params" j in
  let l = to_list param_j in
  let updated_tbls = List.nth l 1 in
  let () = process_notification_all db_name updated_tbls in
  Lwt.return ()


let list_db_h signaler j =
  let open Yojson.Basic.Util in
  Printf.printf "%s\n" (Yojson.Basic.pretty_to_string  j);
  flush stdout;
  let param_j = member "result" j in
  let l = to_list param_j in
  let l = List.sort compare l in
  Lwt.wakeup signaler (List.map (fun db_j -> to_string db_j) l);
  Lwt.return ()


let process_table tab =
  let open Yojson.Basic.Util in
  let c = member "columns" tab in
  match c with
  | `Assoc col ->
     Lwt_list.iter_s (fun (n, v) ->
         Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0) n
       ) col
     >>= fun () ->
     Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
       (": " ^ (Yojson.Basic.pretty_to_string
                  (c)))
  | _ -> Lwt.return ()

let list_tables_h name signaler j =
  let open Yojson.Basic.Util in
  let r_j = member "result" j in
  let tables_j = member "tables" r_j in
  let () = Helpers.add_meta_info j in
  match tables_j with
  | `Assoc tables ->
     Lwt_log.debug ~section ~logger ~location:(__FILE__, __LINE__, 0)
       ("Number of tables: " ^ (Printf.sprintf "%d" (List.length tables)))
     >>= fun () ->
     let t_names = List.map (fun (n, _) ->
                       n
                     ) tables
     in
     let l = List.rev (List.sort compare t_names) in
     Lwt.wakeup signaler l;
     Lwt.return ()
  | _ ->
     Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
       ("Tables not of type assoc: " ^ (Yojson.Basic.pretty_to_string  tables_j))


let find_json_element str name =
  let json = Yojson.Basic.from_string str in
  let rec search v =
    let r = match v with
      | `Int i -> None
      | `String s -> None
      | `Assoc a ->
         let keys = Yojson.Basic.Util.keys v in
         let rec pkey l =
           match l with
             k :: tl ->
              if k = name then
                Some (Yojson.Basic.Util.member k v)
              else
                let f = search (Yojson.Basic.Util.member k v) in
                begin
                  match f with
                  | None ->
                     pkey tl
                  | Some s ->
                     f
                end
           | [] ->
              None
         in
         pkey keys
      | `Bool b -> None
      | `Float f -> None
      | `List l -> None
      | `Null -> None
    in
    r
  in
  search json


let show_select db_name tbl_name signaler j =
  let open Yojson.Basic.Util in
  let only_keys, keys, keys_short, values, values_short =
    Helpers.prepare_result db_name tbl_name j
  in
  let uuid_pos = Helpers.list_pos only_keys "_uuid"  in
  let links = Helpers.create_link_list only_keys ("\"" ^ db_name ^ "\"." ^ tbl_name ^ ".") in
  Lwt.wakeup signaler (uuid_pos, keys, keys_short, only_keys, links, values, values_short);
  Lwt.return ()



let get_databases () =
  let waiter, signaler = Lwt.wait () in
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (list_db_h signaler) in
  let rpc = Helpers.build_jsonrcp_request (`String id) (`List []) "list_dbs" in
  print_endline (Yojson.Basic.to_string rpc);
     let r = Helpers.write_to_db  (Yojson.Basic.to_string rpc) in
     r >>= fun ok ->
     if ok then
       waiter >>= fun l ->
       Lwt.return (l)
     else
       Lwt.return ["get_database_failed"]


let%client get_databases =
  ~%(Eliom_client.server_function [%derive.json: unit]
       (get_databases))

let list_tables name =
  let waiter, signaler = Lwt.wait () in
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (list_tables_h name signaler) in
  let rpc = Helpers.build_jsonrcp_request (`String id) (`List [`String name]) "get_schema" in
  print_endline (Yojson.Basic.to_string rpc);
     let r = Helpers.write_to_db  (Yojson.Basic.to_string rpc) in
     r >>= fun ok ->
     if ok then
       waiter >>= fun l ->
       let table_plus_references_list =
         (* FIXME: Lookup the mapping*)
         List.map (fun tn ->
             let rl = State.get_reference (name ^ "." ^ tn) in
             let rs =
               match rl with
               | [] -> ""
               | e -> List.fold_left (fun s e -> s ^ e ^ " ") "<- " rl
             in
             (tn, rs)
           ) l
       in
       Lwt.return (table_plus_references_list)
     else
       Lwt.return ["list_tables failed", ""]


let%client list_tables =
  ~%(Eliom_client.server_function [%derive.json: string]
       (list_tables))

let list_table (db_name, tbl_name) : (int * string list * string list 
                                      * string list * string list  
                                      * string list list 
                                      * string list list) Lwt.t =
  let waiter, signaler = Lwt.wait () in
  let req = `List [`String db_name;
                   `Assoc [
                       "op", `String "select";
                       "table", `String tbl_name;
                       "where", `List [];
                     ]
              ]
  in

  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (show_select db_name tbl_name signaler) in
  let rpc = Helpers.build_jsonrcp_request (`String id) (req) "transact" in

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    waiter >>= fun l ->
    Lwt.return l
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return (0, [], [], [], [], [[]], [[]])


let%client list_table =
  ~%(Eliom_client.server_function [%derive.json: (string * string) ]
       (list_table))

let show_row tbl signaler j =
  let open Yojson.Basic.Util in
  let str = Printf.sprintf "%s:\n%s\n" tbl (Yojson.Basic.pretty_to_string  j) in
  Lwt.wakeup signaler str;
  Lwt.return ()


let get_uuid_str str =
  let j = Yojson.Basic.from_string str in
  let j_l = Yojson.Basic.Util.to_list j in
  let uuid = if (List.length j_l) = 2 then
               Yojson.Basic.to_string (List.nth j_l 1)
             else
               "NA"
  in
  String.sub uuid 1 ((String.length uuid) - 2)

let list_row (db_name, tbl_name, col_name) : string Lwt.t =
  let tbl_name =   String.sub tbl_name 1 ((String.length tbl_name) - 2) in

  let uuid = Yojson.Basic.from_string col_name in
  let waiter, signaler = Lwt.wait () in
  let req = `List [`String db_name;
                   `Assoc [
                       "op", `String "select";
                       "table", `String tbl_name;
                       (* "where", `List []; *)
                       "where", `List [`List [`String "_uuid"; `String "==" ; uuid]];
                     ]
              ]
  in

  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (show_row tbl_name signaler) in
  let rpc = Helpers.build_jsonrcp_request (`String id) (req) "transact" in

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    waiter >>= fun l ->
    Lwt.return l
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("")


let%client list_row =
  ~%(Eliom_client.server_function [%derive.json: (string * string * string) ]
       (list_row))


let start_monitor_column (db_name, tbl_name, col_name) : string Lwt.t =
  let m_id = db_name ^ tbl_name ^ col_name in
  State.add_monitor m_id db_name tbl_name col_name;
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (mon_init db_name) in
  let () = State.remove_response_handler m_id in
  let () = State.add_response_handler m_id (mon1 db_name) in
  let req = `List [`String db_name; `String m_id;
                   `Assoc [
                       tbl_name,
                       `Assoc [
                           "columns", `List [`String col_name];
                           "select", `Assoc [
                                         "initial", `Bool true;
                                         "insert", `Bool false;
                                         "modify", `Bool true;
                                         "delete", `Bool false;
                                       ];

                         ]
                     ]
              ]
  in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "monitor" in
  print_endline (Yojson.Basic.to_string rpc);

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    Lwt.return "OK"
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")


let%client start_monitor_column =
  ~%(Eliom_client.server_function [%derive.json: (string * string * string) ]
       (start_monitor_column))


let start_monitor_table (db_name, tbl_name ) : string Lwt.t =
  let m_id = db_name ^ tbl_name in
  State.add_monitor m_id db_name tbl_name "";
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (mon_init_table db_name) in
  let () = State.remove_response_handler m_id in
  let () = State.add_response_handler m_id (mon2 db_name) in
  let req = `List [`String db_name; `String m_id;
                   `Assoc [
                       tbl_name,
                       `Assoc [
                           "select", `Assoc [
                                         "initial", `Bool false;
                                         "insert", `Bool true;
                                         "modify", `Bool false;
                                         "delete", `Bool true;
                                       ];

                         ]
                     ]
              ]
  in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "monitor" in
  print_endline (Yojson.Basic.to_string rpc);

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    let () = Printf.printf "HANS: mon table %s%s\n" db_name tbl_name in
    let () = flush stdout in
    Lwt.return "OK"
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")


let%client start_monitor_table =
  ~%(Eliom_client.server_function [%derive.json: (string * string ) ]
       (start_monitor_table))

let start_monitor_table_full (db_name, tbl_name ) : string Lwt.t =
  let m_id = db_name ^ tbl_name in
  State.add_monitor m_id db_name tbl_name "";
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id (mon_init_table db_name) in
  let () = State.remove_response_handler m_id in
  let () = State.add_response_handler m_id (mon3 db_name) in
  let req = `List [`String db_name; `String m_id;
                   `Assoc [
                       tbl_name,
                       `Assoc [
                           "select", `Assoc [
                                         "initial", `Bool true;
                                         "insert", `Bool true;
                                         "modify", `Bool true;
                                         "delete", `Bool true;
                                       ];

                         ]
                     ]
              ]
  in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "monitor" in
  print_endline (Yojson.Basic.to_string rpc);

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    begin
      let s = Printf.sprintf "monitor table %s%s\n" db_name tbl_name in
      Lwt_log.debug ~section ~logger ~location:(__FILE__, __LINE__, 0)
        s >>= fun () ->
      flush stdout;
      Lwt.return "OK"
    end
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")

let%client start_monitor_table_full =
  ~%(Eliom_client.server_function [%derive.json: (string * string ) ]
       (start_monitor_table_full))



let stop_monitor_column (db_name, tbl_name, col_name) : string Lwt.t =
  let m_id = db_name ^ tbl_name ^ col_name in
  State.remove_monitor m_id;
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id mon_remove in
  let () = State.remove_response_handler m_id in
  let req = `List [`String m_id]
  in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "monitor_cancel" in
  print_endline (Yojson.Basic.to_string rpc);

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    Lwt.return "OK"
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")


let%client stop_monitor_column =
  ~%(Eliom_client.server_function [%derive.json: (string * string * string) ]
       (stop_monitor_column))



let stop_monitor_table (db_name, tbl_name ) : string Lwt.t =
  let m_id = db_name ^ tbl_name ^ "" in
  State.remove_monitor m_id;
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id mon_remove in
  let () = State.remove_response_handler m_id in
  let req = `List [`String m_id]
  in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "monitor_cancel" in
  print_endline (Yojson.Basic.to_string rpc);

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    Lwt.return "OK"
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")


let%client stop_monitor_table =
  ~%(Eliom_client.server_function [%derive.json: (string * string) ]
       (stop_monitor_table))

let stop_monitor_table_full (db_name, tbl_name ) : string Lwt.t =
  let m_id = db_name ^ tbl_name ^ "" in
  State.remove_monitor m_id;
  let id = Helpers.get_next_id () in
  let () = State.add_response_handler id mon_remove in
  let () = State.remove_response_handler m_id in
  let req = `List [`String m_id]
  in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "monitor_cancel" in
  print_endline (Yojson.Basic.to_string rpc);

  let r = Helpers.write_to_db (Yojson.Basic.to_string rpc) in
  r >>= fun ok ->
  if ok then
    Lwt.return "OK"
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")

let%client stop_monitor_table_full =
  ~%(Eliom_client.server_function [%derive.json: (string * string) ]
       (stop_monitor_table_full))


let update_element (db_name, tbl_name, col_name, uuid, value) : string Lwt.t =
  let value_json = Yojson.Basic.from_string value in
  let re = Str.regexp "\\\\\"" in
  let id = Helpers.get_next_id () in
  let req = `List [`String db_name;
                   `Assoc [
                       "op", `String "update";
                       "table", `String tbl_name;
                       "where", `List [`List [`String "_uuid"; `String "==" ; `List [`String "uuid"; `String uuid]]];
                       "row", `Assoc [
                                  col_name, value_json;
                                ];
                     ]
              ]
  in
  let () = Printf.printf "%s\n" (Yojson.Basic.to_string req) in
  let rpc = Helpers.build_jsonrcp_request (`String id ) (req) "transact" in
  let r =  (Yojson.Basic.to_string rpc) in
  let r2 = Str.global_replace re "\"" r in

  let r = Helpers.write_to_db r2 in
  r >>= fun ok ->
  if ok then
    Lwt.return "OK"
  else
    Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
      "No out channel for database connection" >>= fun () ->
    Lwt.return ("Failed: no DB connection")


let%client update_element =
  ~%(Eliom_client.server_function [%derive.json: (string * string * string * string * string ) ]
       (update_element))



let main () =
  Lwt_log.Section.set_level section Lwt_log.Info;
  Lwt_log.Section.set_level Handlers.section Lwt_log.Info;

  let host =
    try
      Sys.getenv "OVSDB_HOST"
    with Not_found ->
      "127.0.0.1"
  in
  let port =
    try
      int_of_string  (Sys.getenv "OVSDB_PORT")
    with Not_found | Failure _ ->
      6640
  in

  let s = Printf.sprintf "Database at %s:%d\n" host port in
  Lwt_log.debug ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->

  let () = Handlers.init_request_handlers () in
  let () = Lwt.async (receiver_th host port) in
  Lwt.async (notify_th );

  Lwt.return ()
