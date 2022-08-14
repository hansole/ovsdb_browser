(**
This is the frontend of the application.
It has most of the presentation logic
 *)
[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

[%%client
    open Js_of_ocaml
    open Js_of_ocaml_lwt
]

module Ovsdb_browser_app =
  Eliom_registration.App (
    struct
      let application_name = "ovsdb_browser"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%client main_service = ~%main_service

let _ =  Backend.main ()

(**
 Get ther 'pure' uuid from a "uuid" string
 *)
let%shared get_uuid_str str =
  let len = String.length str in
  let rec loop p2 pos =
    if pos = 0 then
      0,1
    else
      let c = String.get str pos in
      if c = '"' then
        if p2 = 0 then
          loop pos (pos -1)
        else
          pos, p2
      else
        loop p2 (pos - 1)
  in
  let p1, p2 = loop 0 (len -1) in
  String.sub str (p1+1) ((p2 -1) - p1)

    (* open Js_of_ocaml
     * open Js_of_ocaml_lwt *)

let%shared button msg msg2 db f v =
  let btn =
    Eliom_content.Html.
      (D.button ~a:[D.a_class ["button"]] [D.txt msg])
  in
  let cur = ref msg in
  ignore [%client
    ((Lwt.async @@ fun () ->
      Lwt_js_events.clicks
        (Eliom_content.Html.To_dom.of_element ~%btn)
        (fun _ _ ->
          let () =
            if !(~%cur) = ~%msg then
              ~%cur := ~%msg2
            else
              ~%cur := ~%msg
          in
          let btn = Eliom_content.Html.To_dom.of_element ~%btn in
          btn##.innerHTML := (Js.string !(~%cur));
          ~%f ~%v !(~%cur) ~%db ))
     : unit)
  ];
  btn


let%shared my_match sub str =
  let sub_len = String.length sub in
  let str_len = String.length str in
  let rec loop sub_pos str_pos =
    if sub_pos >= sub_len  then
      true
    else if str_pos >= str_len then
      false
    else
      let str_ch = String.get str str_pos in
      let sub_ch = String.get sub sub_pos in
      if str_ch = sub_ch then
        loop (sub_pos + 1) (str_pos + 1)
      else
        loop 0 (str_pos + 1)
  in
  loop 0 0

let%client active_monitors = ref []

let%client add_active_monitor (db : string) (tb : string) (col : string) =
  active_monitors := (db, tb, col)::(!active_monitors)


let%client remove_active_monitor (db : string) (tb : string) (col : string) =
  active_monitors := List.filter (fun (db_, tb_, col_) ->
                         if (db = db_) && (tb = tb_) && (col = col_) then
                           false
                         else
                           true
                       )(!active_monitors)



let%client get_and_remove_active_monitor_table db tb =
  let active = List.filter (fun (db_, tb_, col_) ->
                   if (db = db_) && (tb = tb_) then
                     true
                   else
                     false
                 )(!active_monitors) in
  let () = List.iter (fun (db_, tb_, col_) ->
               remove_active_monitor db_ tb_ col_
             ) active
  in
  active

let%client get_and_remove_active_monitor_db db =
  let active = List.filter (fun (db_, tb_, col_) ->
                   if (db = db_)  then
                     true
                   else
                     false
                 )(!active_monitors) in
  let () = List.iter (fun (db_, tb_, col_) ->
               remove_active_monitor db_ tb_ col_
             ) active
  in
  active

let%shared setup_change_popup append_link content sf db tname col uuid_only =
  ignore
    [%client
        ((Lwt.async @@
            fun () ->
            Lwt_js_events.clicks
              (Eliom_content.Html.To_dom.of_element ~%append_link)
              (fun _ _ ->
                let ap = Eliom_content.Html.To_dom.of_element ~%append_link in
                let row_val = Js.to_string ap##.title in
                let len = String.length row_val in
                let rows, cols =
                  if len < 100 then
                    2, 40
                  else if len < 300 then
                    10, 60
                  else
                    20, 60
                in

                let input_text = Html.D.textarea ~a:[a_name "a"; a_rows rows; a_cols cols]
                                   (txt row_val) in
                let submit = Eliom_content.Html.D.Form.input ~input_type:`Submit ~value:"Submit" Eliom_content.Html.D.Form.string in
                let heading = (~%db ^ "." ^ ~%tname ^ "." ^ ~%col
                               ^ " uuid: " ^ ~%uuid_only ) in
                let aaa = p [txt heading;
                             br ();
                             input_text;
                             br ();
                             submit;]
                in
                ignore [
                    ((Lwt.async @@
                        fun () ->
                        Js_of_ocaml_lwt.Lwt_js_events.clicks
                          (Eliom_content.Html.To_dom.of_element submit)
                          (fun _ _ ->
                            let vi = Eliom_content.Html.To_dom.of_textarea (input_text) in
                            let%lwt r = Backend.update_element
                                          (~%db, ~%tname, ~%col,
                                           ~%uuid_only, (Js.to_string vi##.value)) in
                            Lwt.return_unit
                          )
                        : unit))
                  ];
                let%lwt _=
                  Ot_popup.popup
                    ~close_button:[ Ot_icons.F.close () ]
                    (fun _ -> Lwt.return @@
                                aaa )
                in
                Lwt.return_unit)
            : unit))
    ]

let%shared map_table l =
  let l =
    Eliom_shared.ReactiveData.RList.map
      [%shared
          ((fun (uuid_pos, header, row_f , row, cols, links, (db : string), (tname : string)) ->
            try
              let uuid = (List.nth row_f uuid_pos) in
              let uuid_only = get_uuid_str uuid in
              let dbn = db ^ "." ^ tname in
              let () = Backend.add_row_index dbn uuid_only in
              let hrow =
                if header then
                  begin
                    List.mapi (fun i sf ->
                        let s = List.nth row i in
                        let col = List.nth cols i in
                        let header_row_element =
                          if col != "_uuid" then
                            let has_uuid = (my_match "uuid" sf) && (not (my_match "_uuid" sf)) in
                            let table_type =
                              if has_uuid then
                                "w3-monitor-off-reference"
                              else
                                "w3-monitor-off"
                            in
                            let append_link =
                              div ~a:[Eliom_content.Html.D.a_class
                                        [table_type]]  [txt s]
                            in
                            ignore [%client
                                       ((Lwt.async @@
                                           fun () ->
                                           Lwt_js_events.clicks
                                             (Eliom_content.Html.To_dom.of_element ~%append_link)
                                             (fun _ _ ->
                                               let div = Eliom_content.Html.To_dom.of_div ~%append_link in
                                               let _ =
                                                 if div##.className =  (Js.string "w3-monitor-off") then
                                                   begin
                                                     div##.className :=
                                                       Js.string "w3-monitor-on";
                                                     let () = add_active_monitor ~%db ~%tname ~%col in
                                                     let%lwt r = Backend.start_monitor_column (~%db, ~%tname, ~%col) in
                                                     Lwt.return ()
                                                   end
                                                 else if div##.className =  (Js.string "w3-monitor-off-reference") then
                                                   begin
                                                     div##.className :=
                                                       Js.string "w3-monitor-on-reference";
                                                     let () = add_active_monitor ~%db ~%tname ~%col in
                                                     let%lwt r = Backend.start_monitor_column (~%db, ~%tname, ~%col) in
                                                     Lwt.return ()
                                                   end
                                                 else if div##.className =  (Js.string "w3-monitor-on-reference") then
                                                   begin
                                                     div##.className :=
                                                       Js.string "w3-monitor-off-reference";
                                                     let () = add_active_monitor ~%db ~%tname ~%col in
                                                     let%lwt r = Backend.start_monitor_column (~%db, ~%tname, ~%col) in
                                                     Lwt.return ()
                                                   end
                                                 else
                                                   begin
                                                     div##.className :=
                                                       Js.string "w3-monitor-off";
                                                     let () = remove_active_monitor ~%db ~%tname ~%col in
                                                     let%lwt r = Backend.stop_monitor_column (~%db, ~%tname, ~%col) in
                                                     Lwt.return ()
                                                   end
                                               in
                                               Lwt.return ()
                                             )
                                        )
                                        : unit)
                              ];
                            [append_link]
                          else
                            [txt s]
                        in
                        Eliom_content.Html.D.td
                          ~a:[
                            a_title sf;
                          ] header_row_element

                      ) row_f
                  end
                else
                  List.mapi (fun i sf->
                      let s = List.nth row i in
                      let link = List.nth links i in
                      let col = List.nth cols i in
                      let uuid_link =
                        if link = "" then
                              let append_link = txt s
                              in
                              append_link
                        else
                          begin
                            let has_uuid = (my_match "uuid" sf) in
                            if has_uuid then
                              let append_link =
                                  div ~a:[Eliom_content.Html.D.a_class
                                            ["w3-text-blue"]] [txt s]
                              in
                              ignore [%client
                                         ((Lwt.async @@
                                             fun () ->
                                             Lwt_js_events.clicks
                                               (Eliom_content.Html.To_dom.of_element ~%append_link)
                                               (fun _ _ ->
                                                 let%lwt r = Backend.list_row (~%db, ~%link, ~%sf) in
                                                 Eliom_lib.alert "%s" r;
                                                 Lwt.return ()
                                               )
                                          )
                                          : unit)
                                ];

                              append_link
                            else
                              txt s
                          end
                      in
                      let id = (db ^ "." ^ uuid_only ^ "." ^ col) in
                      let td = Eliom_content.Html.D.td
                        ~a:[
                          a_id id ;
                          a_title sf;
                        ] [uuid_link]
                      in
                      let () =
                        (* if link = "" then *)
                          let () = setup_change_popup td uuid_link sf db tname col uuid_only in
                          ()
                        (* else
                         *   () *)
                      in
                      td

                    ) row_f
              in
              Eliom_content.Html.(
                D.tr ~a:[a_id uuid_only; ] hrow;
              )
            with _ ->
              Eliom_content.Html.(
                   D.tr [
                       D.th [txt ""]
                     ]
              ) : _ -> _)
          )
      ]
      l
  in
  l

let%client layout_table my_tables ha2 monitor active_header v cur db =
  if (cur = "-" ) || (cur = "Stop") then (* Value is change before call*)
    let my_table = Eliom_content.Html.To_dom.of_table my_tables in
    my_table##.id := Js.string (db ^ "." ^ v);
    let%lwt t = Backend.list_table (db, v) in
    let%lwt r = monitor (db, v) in
    let () = add_active_monitor db v "" in
    let () = Backend.add_row_table (db ^ "." ^ v) ha2 in
    let () = Eliom_shared.ReactiveData.RList.set ha2 [] in
    let uuid_pos, header_full, header, keys, links, elements_full, elements = t in
    Eliom_shared.ReactiveData.RList.snoc
      (uuid_pos, active_header, header_full, header, keys, links, db, v) ha2;
    let () = if elements = [[]] then
               ()
             else
               List.iter2 (fun tnf tn ->
                   Eliom_shared.ReactiveData.RList.snoc
                     (uuid_pos, false, tnf, tn, keys, links, db, v) ha2
                 ) elements_full elements
    in
    Lwt.return ()
  else
    let m = get_and_remove_active_monitor_table db v in
    let key = db ^ "." ^ v in
    let () = Backend.drop_row_index key in
    let%lwt r = Backend.stop_monitor_table (db, v) in
    let r = Lwt_list.iter_s (fun (db, tb, col) ->
                let%lwt r = Backend.stop_monitor_column (db, tb, col) in
                Lwt.return ()
              ) m
    in
    r >>= fun () ->
    let () = Eliom_shared.ReactiveData.RList.set ha2 [] in
    Lwt.return ()


let map_dbs db l =
  let l =
    Eliom_shared.ReactiveData.RList.map
      [%shared
          ((fun (sh, (s, (table_ref : string))) ->
            let my_tables, ha2 =
              let l, h = Eliom_shared.ReactiveData.RList.create [] in
              let l = map_table l in
              let rul = Html.R.table l in
              rul, h
            in
            let btn =
                  button "+" "-" ~%db
                    [%client (layout_table ~%my_tables ~%ha2 Backend.start_monitor_table true)] s
            in
            let btn2 =
                  button "Monitor" "Stop" ~%db
                    [%client (layout_table ~%my_tables ~%ha2 Backend.start_monitor_table_full false)] s
            in
            if sh then
              Eliom_content.Html.(
              D.li [b [D.txt s]; btn; btn2; D.txt table_ref;
                    span ~a:[a_id (~%db ^ "." ^ s) ][D.txt  ""]; div [my_tables]]
              )
            else
              Eliom_content.Html.(
              D.li [b [D.txt s];
                    span ~a:[a_id (~%db ^ "." ^ s) ][D.txt  ""]; div [my_tables]]
              )

          ))
      ]
      l
  in
  l


let map_dbs2 db l =
  let l =
    Eliom_shared.ReactiveData.RList.map
      [%shared
          ((fun (sh, (s, (table_ref : string))) ->
            let my_tables, ha2 =
              let l, h = Eliom_shared.ReactiveData.RList.create [] in
              let l = map_table l in
              let rul = Html.R.table l in
              rul, h
            in
            let dbs = ~%db in
            ignore [%client
                       (Lwt.async @@ fun () ->
                                    (layout_table ~%my_tables ~%ha2 Backend.start_monitor_table_full false ~%s "-" ~%dbs) : unit)
              ];
            if sh then
              Eliom_content.Html.(
              D.li [b [D.txt s]; span ~a:[a_id (~%db ^ "." ^ s) ][D.txt  ""]; div [my_tables]]
              )
            else
              Eliom_content.Html.(
              D.li [b [D.txt s]; span ~a:[a_id (~%db ^ "." ^ s) ][D.txt  ""]; div [my_tables]]
              )

          ) : _ -> _)
      ]
      l
  in
  l



let%client layout_databases h h2 db v cur _ =
  if (cur = "-" ) || (cur = "Stop") then (* Value is change before call*)
    let fast_c = Eliom_comet.Configuration.new_configuration () in
    let () = Eliom_comet.Configuration.set_always_active fast_c true in
    let () = Eliom_shared.ReactiveData.RList.cons (false, ("Please wait...", "")) h in
    let%lwt t = Backend.list_tables v in
    let () = Eliom_shared.ReactiveData.RList.set h [] in
    let () = List.iter (fun tn ->
                 Eliom_shared.ReactiveData.RList.cons (true, tn) h2
               ) t
    in
    Lwt.return ()
  else
    let m = get_and_remove_active_monitor_db db in
    let r = Lwt_list.iter_s (fun (db, tb, col) ->
                let%lwt r = Backend.stop_monitor_column (db, tb, col) in
                let key = db ^ "." ^ tb in
                let () = Backend.drop_row_index key in
                Lwt.return ()
              ) m
    in
    r >>= fun () ->
    let () = Eliom_shared.ReactiveData.RList.set h2 [] in
    Lwt.return ()

let () =
  Ovsdb_browser_app.register
    ~service:main_service
    (fun () () ->
      let%lwt dbs = Backend.get_databases ()
      in
      let databases =
        List.map (fun db ->
            let l, h = Eliom_shared.ReactiveData.RList.create [] in
            let l = map_dbs db l in
            let l2, h2 = Eliom_shared.ReactiveData.RList.create [] in
            let l2 = map_dbs2 db l2 in
            let btn =
              button "+" "-" db
                [%client
                    (layout_databases ~%h ~%h ~%db)
                ] db
            in
            let btn2 =
              button "Monitor" "Stop" db
                [%client
                    ((layout_databases ~%h ~%h2 ~%db)
                     : string -> string -> string -> unit Lwt.t)
                ] db
            in
            let rul = [Html.R.ul l] in
            let rul2 = [Html.R.ul l2] in
            div [span [txt db; btn; btn2; span  ~a:[a_id db ] [txt  ""]  ];
                div rul ; div rul2 ]
          ) dbs
      in
      Backend.listen () >>= fun () ->
      Lwt.return
        (Eliom_tools.D.html
           ~title:"ovsdb browser"
           ~css:[["css";"ovsdb_browser.css"]]
           Html.(D.body ([
                         D.h1 [txt "ovsdb browser (0.0.1)"];
                         D.span [txt "Databases "];
                           span  ~a:[a_id "_json_rpc_speed"  ] [txt  ""];
                       ]
                           @ databases
                   )
    )));
