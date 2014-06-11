{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_react
  open Html5
  open Html5.D

  module Rooms = Map.Make(String)
  module Users = Set.Make(String)
}}

{server{
module Chat_app =
  Eliom_registration.App (
    struct
      let application_name = "chat"
    end)

let chat_service =
  Eliom_service.App.service
    ~path:["chat"]
    ~get_params: (Eliom_parameter.suffix (Eliom_parameter.string "name"))
    ()

let initial_rooms = Rooms.singleton "testraum" Users.empty

let rooms_s, set_rooms_s = React.S.create initial_rooms
let client_room_s = S.Down.of_react rooms_s

let create_room str =
  let _ = set_rooms_s (Rooms.add str Users.empty (React.S.value rooms_s)) in
  Lwt.return ()
let rpc_create_room = server_function Json.t<string> create_room

let home_service =
  Eliom_service.App.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()

let skeleton body_content =
  Lwt.return
    (Eliom_tools.D.html
       ~title: "Ocsigen Chat"
       ~css:[["css";"bootstrap.css"]]
       (body [div ~a:[a_class ["container"]]
                  body_content])
    )

let home =
  Chat_app.register
    ~service:home_service
    (fun () () ->
     let room_input = D.string_input
                        ~a:[a_class ["form-control"];
                            a_placeholder "Room name"]
                        ~input_type:`Text ()
     in
     let send_data = {{fun ev -> (
                         let input = To_dom.of_input %room_input in
                         let room = (Js.to_string input##value) in
                         Lwt.async
                           (fun () ->
                            %rpc_create_room room)
                     )}}
     in
     let room_button = D.button
                         ~a:[a_class ["btn btn-primary"]; a_onclick send_data]
                         ~button_type:`Button
                         [pcdata "Create room"]
     in
     skeleton
       [
         h1 [pcdata "Ocsigen Chat"];
         p [strong [pcdata "Currently available chat rooms:"] ];
         C.node {{R.node
                    (React.S.map
                       (fun rooms ->
                        (ul (Rooms.fold
                               (fun r u acc ->
                                let us = string_of_int (Users.cardinal u) in
                                (acc@[li [pcdata (r^" ("^us^" subscribed users)")]
                               ]))
                               rooms [])))
                     %client_room_s)
                }};
         div ~a:[a_class ["form-inline"]]
             [div ~a:[a_class ["form-group"]] [room_input];
              pcdata " ";
              room_button
             ]
       ]
    )

let chat =
  Chat_app.register
    ~service:chat_service
    (fun room () ->
     Lwt.return
       (Eliom_tools.D.html
          ~title:"chat"
          ~css:[["css";"chat.css"]]
          Html5.D.(body [
                       h2 [pcdata "Welcome to heaven!"; pcdata room];
    ])))
}}
