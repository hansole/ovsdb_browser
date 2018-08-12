(**
Functions that comunicate with the ovsdb server
and helps process JSON RPC data
 *)
open Yojson.Basic

let section = Lwt_log.Section.make "main"
let logger = Lwt_log.channel
               ~template:"$(name): $(section): $(loc-file): $(loc-line): $(message)"
               ~close_mode:`Keep ~channel:Lwt_io.stderr ()

let (>>=) = Lwt.bind

let max_buf = 65536 * 128
let read_pos = ref 0
let check_pos = ref 0
let inch_buf = Bytes.create max_buf
let inch_depth = ref 0

let show_extra = ref false

let read_next_json_object inch =
  let rec loop () =
    if !read_pos > !check_pos then
      let c = Bytes.get inch_buf !check_pos in
      incr check_pos;
      match c with
      | '{' -> incr inch_depth; loop ()
      | '}' -> if !inch_depth == 1 then
                 begin
                   decr inch_depth;
                   let rets = Bytes.sub_string inch_buf 0 !check_pos in
                   begin
                     if !read_pos > !check_pos then
                       begin
                         Bytes.blit inch_buf (!check_pos) inch_buf 0 (!read_pos - !check_pos);
                         read_pos := !read_pos - !check_pos;
                         begin
                           if ((Bytes.get inch_buf 0) != '{') then
                             begin
                               let () = Printf.printf "ERROR. Not starting with '{'\n" in
                               flush stdout
                             end
                           else
                             ()
                         end
                       end
                     else
                       begin
                         read_pos := 0
                       end
                   end;
                   check_pos := 0;
                   Lwt.return rets
                 end
               else
                 begin
                   decr inch_depth;
                   loop ()
                 end
      | a ->
         begin
           loop ()
         end
    else
      Lwt.catch (fun () ->
      let r = Lwt_io.read_into inch inch_buf (!read_pos) (4096) in
          r >>= fun len ->
          read_pos := !read_pos + len;
          loop ()
    ) (function
          | e ->
             let s = Printf.sprintf "failed to read from db: " in
             Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->

             Lwt.return ""
        )
  in
  loop ()


let get_id_gen () =
  let next_id = ref 0 in

  let get_next_id () =
    let () = next_id := !next_id +1 in
    Printf.sprintf "%d" (!next_id)
  in
  get_next_id

let connect_to_db host port =
  let rec loop () =
    let timeout =
      let%lwt () = Lwt_unix.sleep 5. in
      Lwt.return None
    in
    let connect =
      let uhost = Unix.inet_addr_of_string host in
      let sockaddr = Unix.ADDR_INET (uhost, port) in
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let con = Lwt_unix.connect sock sockaddr in
      Lwt.catch (fun () ->
          con >>= fun () ->
          let outch = Lwt_io.of_fd Lwt_io.Output sock  in
          let inch = Lwt_io.of_fd Lwt_io.Input sock  in
          Lwt.return (Some (outch, inch))
        ) (function
          | Unix.Unix_error (error, fn_name, param_name) ->
             let s = Printf.sprintf "failed connect %s %s %s"
                       (Unix.error_message error) fn_name param_name in
             Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->

             Lwt.return None
          | e -> Lwt.fail e )
    in
    let%lwt r = Lwt.pick [timeout; connect] in
    match r with
    | None ->
       let s = Printf.sprintf "Connect to db at %s:%d failed. Will retry...." host port in
       Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->
       let%lwt () = Lwt_unix.sleep 5. in
       loop ()
    | Some s ->
       Lwt.return s

  in
  loop ()


let write_to_db str =
  match State.get_outch () with
    None ->
     Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0)
       "No out channel for database connection" >>= fun () ->
     Lwt.return false
  | Some outch ->
     Lwt.catch (fun () ->
         let r = Lwt_io.write outch (str) in
         r >>= fun () ->
         Lwt.return true
       ) (function
          | e ->
             let s = Printf.sprintf "failed to write to db: " in
             Lwt_log.warning ~section ~logger ~location:(__FILE__, __LINE__, 0) s >>= fun () ->

             Lwt.return false
        )

let get_next_id = get_id_gen ()

let colapse_string s =
  if (String.length s) < 21 then
    s
  else
    let first = String.sub s 0 15 in
    let last = String.sub s (String.length s - 3) 3 in
    (first ^ "..." ^ last)

let rec value_to_short_string v =
  let r = match v with
    | `Int i -> let s = Printf.sprintf "%d" i in s
    | `String s -> s
    | `Assoc a -> "jsonObject"
    | `Bool b -> if b = true then "true" else "false"
    | `Float f -> let s = Printf.sprintf "%f" f in s
    | `List l ->
       let len = (List.length l) in
       if len = 2 then
         let key = List.hd l in
         match value_to_short_string key with
         | "uuid" -> value_to_short_string (List.nth l 1)
         | "set" ->
            let e = List.nth l 1 in
            let el = Util.to_list e in
            let s = Printf.sprintf "Set[%d]" (List.length el ) in s
         | "map" ->
            let e = List.nth l 1 in
            let el = Util.to_list e in
            let s = Printf.sprintf "Map[%d]" (List.length el ) in s
         | _ ->
            let s = Printf.sprintf "List[%d]" len in s
       else
         let s = Printf.sprintf "List[%d]" len in s
    | `Null -> "null"
  in
  colapse_string r


let list_pos keys str =
  let rec loop keys pos =
    match keys with
    | key :: tl ->
       if key = str then
         pos
       else
         loop tl (pos + 1)
    | [] -> 0
  in
  loop keys 0


let process_row_l dbn tbl_name rows_l =
  let only_keys_from_meta = State.get_meta_cols (dbn ^ "." ^ tbl_name) in
  let only_keys = List.rev ("_uuid"::"_version"::(List.rev only_keys_from_meta)) in
  let keys_short = List.map colapse_string only_keys in
  let keys = List.map (fun cname ->
                 cname ^ ":" ^ (State.get_meta (dbn ^ "." ^ tbl_name ^ "." ^ cname))
               )
               only_keys
  in

  let values_j =
    List.map (fun row ->
        let vals = List.map (fun k ->
                       Util.member k row) only_keys in
        vals
      ) rows_l
  in

  let values_short =
    List.map (fun row ->
        List.map value_to_short_string row
      ) values_j
  in

  let values =
    List.map (fun row ->
        List.map pretty_to_string row
      ) values_j
  in
  (only_keys, keys, keys_short, values, values_short)


let prepare_result db_name tbl_name json =
  let dbn = "\"" ^ db_name ^ "\"" in
  let result = (Util.member "result" json) in
  let result_l = Util.to_list result in
  let rows = (Util.member "rows" (List.hd result_l)) in
  let rows_l = Util.to_list rows in
  if List.length rows_l > 0 then
    process_row_l dbn tbl_name rows_l
  else
    let only_keys_from_meta = State.get_meta_cols (dbn ^ "." ^ tbl_name) in
    let only_keys = List.rev ("_uuid"::"_version"::(List.rev only_keys_from_meta)) in
    let keys = List.map (fun cname ->
                   cname ^ ":" ^ (State.get_meta (dbn ^ "." ^ tbl_name ^ "." ^ cname))
                 )
                 only_keys
    in

    let keys_short = List.map colapse_string only_keys in
    only_keys, keys, keys_short, [[]], [[]]


let find_json_element json name =
  let rec search v =
    let r = match v with
      | `Int i -> None
      | `String s -> None
      | `Assoc a -> let keys = Util.keys v in
                    let rec pkey l =
                      match l with
                        k :: tl ->
                         if k = name then
                           Some (Util.member k v)
                         else
                           let f = search (Util.member k v) in
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

let find_json_element_of_string str name =
  let json = Yojson.Basic.from_string str in
  find_json_element json name

let add_meta_info json =
  let param_j = Util.member "result" json in
  let name_db = to_string (Util.member "name" param_j) in
  let tables = Util.member "tables" param_j in
  let table_names = Util.keys tables in
  let () = List.iter (fun name_tbl ->
              let t = Util.member name_tbl tables in
              let cols = Util.member "columns" t in
              let column_names = Util.keys cols in
              let () = State.add_meta_cols (name_db ^ "." ^ name_tbl) column_names in
              List.iter (fun cname ->
                  let col = Util.member cname cols in
                  let key = name_db ^ "." ^ name_tbl ^ "." ^ cname in
                  let () = State.add_meta key (pretty_to_string col) in
                  let () =
                    match find_json_element col "refTable" with
                    | None -> ()
                    | Some table_referenced ->
                       let name_db_pure =  String.sub name_db 1 ((String.length name_db) - 2) in
                       let tr = (to_string table_referenced) in
                       let name_tr_pure =  String.sub tr 1 ((String.length tr) - 2) in

                       State.add_reference (name_db_pure ^ "." ^ name_tr_pure) (name_tbl ^ "." ^ cname)
                  in
                  ()
              ) column_names
            ) table_names
  in
  ()


let create_link_list keys pre =
  let is = List.map (fun key ->
               let s = (State.get_meta (pre ^ key)) in
               if s = "N/A" then
                 ""
               else
                 match (find_json_element_of_string s "refTable") with
                 | None ->
                    ""
                 | Some s -> to_string s
             ) keys
  in
  is

let build_jsonrcp_request id params rpc_method =
  let json_id =
    ["id", id]
  in
  let json_params =
      ["params", params]
  in
  `Assoc ([
    "method", `String rpc_method;
  ] @ json_params @ json_id)
