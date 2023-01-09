(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright 2016 by LexiFi.                                                                         *)

(* Inspired from https://github.com/janestreet/incr_dom/blob/master/example/incr_decr/counters.ml *)

(*
    opam install ocaml-vdom

    foreman start -f Procfile.dev

*)
(*
let greet_user (name : string) (id : int) =
  Printf.sprintf "Hello, %s [%d]" name id
;;

let add_user (name : string) (id : int) (is_admin : bool) =
  Printf.sprintf "Added user %s with id %d. IsAdmin? %b" name id is_admin
;;

let greet_user_route () = Routes.(s "user" / str / int /? nil)
let add_user_route () = Routes.(s "user" / str / int / bool / s "add" /? nil)

let router =
  let open Routes in
  Routes.one_of
    [ greet_user_route () @--> greet_user; add_user_route () @--> add_user ]
;; *)

module R = struct
  open Routes

  let sum a b = Printf.sprintf "Sum of %d and %d = %d" a b (a + b)
  let id_handler id = Printf.sprintf "Requested user with id %d" id

  let admin_handler a =
    if a then
      "User is admin"
    else
      "User is not an admin"
  ;;

  let user () = s "user"
  let user_and_id = user () / int /? nil
  let user_and_admin = user () / bool /? nil
  let q = s "confusing" /? nil

  let routes =
    one_of
      [ route (s "hi" /? nil) "Hello, World"
      ; route (s "hello" / s "from" / s "routes" /? nil) "Hello, Routes"
      ; route (s "sum" / int / int /? nil) sum
      ; route user_and_id id_handler
      ; route user_and_admin admin_handler
      ; route q "Foobar"
      ]
  ;;
end

let unwrap_result = function
  | Routes.NoMatch -> "No match"
  | FullMatch r -> r
  | MatchWithTrailingSlash r -> r
;;

let testing () =
  let targets =
    [ "sum/12/127"
    ; "/hi"
    ; "/hello/from/routes"
    ; "/user/121"
    ; "user/false"
    ; "confusing/"
    ; "confusing"
    ]
  in
  List.iter
    (fun target ->
      print_endline (unwrap_result @@ Routes.match' ~target R.routes))
    targets
;;

open Vdom

type model = int
type msg = Inc | Dec | Reset

let update (model : model) (msg : msg) =
  (* let ensure_pos n = if n < 0 then 0 else n in *)
  print_endline "updating...";
  testing ();
  match msg with
  | Inc -> model + 1
  | Dec ->
      if model > 0 then
        model - 1
      else
        model
  | Reset -> 0
;;

let init = 0
let btn txt msg = input [] ~a:[ onclick (fun _ -> msg); type_button; value txt ]

module V = Vdom

let p = V.elt "p"
let h2 = V.elt "h2"

let view model =
  (* ; input [] ~a:[ onclick (fun _ -> Inc); type_button; value "+" ] *)
  div
    [ h2 [ text "An h2 title" ]
    ; p [ text "A p tag" ]
    ; btn "-" Dec
    ; text (string_of_int model)
    ; btn "+" Inc
    ; btn "RESET" Reset
    ]
;;

let app = simple_app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)
;;

let () = Window.set_onload window run