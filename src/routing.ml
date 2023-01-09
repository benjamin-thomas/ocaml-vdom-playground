[@@@warning "-32-37"]

module V = Vdom

type page = Home | About
type model = { curr_page : page }
type msg = ChangedPage of page

let init = { curr_page = Home }

let update (_model : model) (msg : msg) =
  match msg with
  | ChangedPage page -> { curr_page = page }
;;

let p = V.elt "p"
let h2 = V.elt "h2"
let ul = V.elt "ul"
let li = V.elt "li"

let frag_of_page = function
  | Home -> "#/home"
  | About -> "#/about"
;;

let new_page txt page =
  V.elt "a"
    [ V.text txt ]
    ~a:
      [ V.onclick (fun _ -> ChangedPage page)
      ; V.attr "href" @@ frag_of_page page
      ]
;;

let view model =
  V.div
    [ h2 [ V.text "An h2 title" ]
    ; p
        [ V.text "A p tag"
        ; V.div
            [ p [ V.text @@ "Current frag: " ^ frag_of_page model.curr_page ] ]
        ]
    ; ul
        [ li [ new_page "Go to home" Home ]
        ; li [ new_page "Go to about" About ]
        ]
    ]
;;

let app = V.simple_app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)
;;

let () = Window.set_onload window run