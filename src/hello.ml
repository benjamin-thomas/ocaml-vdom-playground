module V = Vdom

type model = int
type msg = Inc

let init = 0

let update (model : model) (msg : msg) =
  match msg with
  | Inc -> model + 1
;;

let p = V.elt "p"
let h2 = V.elt "h2"

let btn txt msg =
  V.input [] ~a:[ V.onclick (fun _ -> msg); V.type_button; V.value txt ]
;;

let view _model =
  V.div
    [ h2 [ V.text "An h2 title" ]
    ; p [ V.text "A p tag"
    ; V.div [ btn "+" Inc ] ]
    ] [@@ocamlformat "disable"]

let app = V.simple_app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)
;;

let () = Window.set_onload window run