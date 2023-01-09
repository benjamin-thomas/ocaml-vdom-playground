[@@@warning "-32-37"]

module V = Vdom

type page = Home | About
type data = { page : page }
type model = Loading | Loaded of data
type msg = ChangedPage of page

open Js_of_ocaml
module Html = Dom_html

(* let frag = Dom_html.window ##. location ##. hash |> Js.to_string in *)
type frag = Frag of string

let page_of_frag (Frag s) =
  match s with
  | "#/home" -> Home
  | "#/about" -> About
  | "" -> Home
  | _ -> failwith "Impossible"
;;

let frag () = Frag (Html.window##.location##.hash |> Js.to_string)
let change_page page = V.return @@ Loaded { page }
let init = change_page (page_of_frag @@ frag ())

let update (_model : model) (msg : msg) =
  match msg with
  (* | ChangedPage page -> V.return @@ Loaded { page } *)
  | ChangedPage page -> change_page page
;;

let p = V.elt "p"
let h2 = V.elt "h2"
let ul = V.elt "ul"
let li = V.elt "li"

let frag_of_page = function
  | Home -> Frag "#/home"
  | About -> Frag "#/about"
;;

let href_of_frag (Frag f) = V.attr "href" @@ f

let new_page txt page =
  V.elt "a"
    [ V.text txt ]
    ~a:
      [ V.onclick (fun _ -> ChangedPage page)
      ; href_of_frag @@ frag_of_page page
      ]
;;

let view_loaded data =
  let (Frag x) = frag_of_page data.page in
  V.div
    [ h2 [ V.text "An h2 title" ]
    ; p [ V.text "A p tag"; V.div [ p [ V.text @@ "Current frag: " ^ x ] ] ]
    ; ul
        [ li [ new_page "Go to home" Home ]
        ; li [ new_page "Go to about" About ]
        ]
    ]
;;

let view model =
  match model with
  | Loading -> V.div [ V.text "Initializing..." ]
  | Loaded data -> view_loaded @@ data
;;

let app = V.app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)
;;

let () = Window.set_onload window run