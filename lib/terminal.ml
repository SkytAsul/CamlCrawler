open Notty
open Notty.I
open Notty_unix
open Notty.Unescape

module Control = struct
  type 'a t = {
    label : string;
    data : 'a
  }

  let create label data = { label; data }

  let get_label t = t.label
end

type t = {
  term : Term.t;
  selected : int
}

type 'a event_result = | Stop | Continue of t | Control of t * 'a

let create () = {term = Term.create (); selected = 0}

let get_dimensions t = Term.size t.term

let rec create_controls_image t i (controls : 'a Control.t list) = match controls with
| [] -> I.empty
| head::tail ->
  let image = if i == t.selected then string (A.st A.bold) ("> " ^ head.label) else string A.empty ("  " ^ head.label) in
  let next = create_controls_image t (i+1) tail in
  if next == I.empty then image else image <-> next

let create_image t content controls =
  let controls_img = create_controls_image t 0 controls in
  let space = void 1 (snd (Term.size t.term) - (height content) - (height controls_img)) in
  content <-> space <-> controls_img

let draw_image t content controls =
  Term.image t.term (create_image t content controls)

let handle_events t (controls : 'a Control.t list) = match Term.event t.term with
| `End | `Key(`Escape, []) -> Stop
| `Key(`Arrow(`Down), []) -> Continue(if t.selected == List.length controls - 1 then t else { t with selected = t.selected + 1 })
| `Key(`Arrow(`Up), []) -> Continue(if t.selected == 0 then t else { t with selected = t.selected - 1 })
| `Key(`Enter, []) -> Control({ t with selected = 0 }, (List.nth controls t.selected).data)
| _ -> Continue(t)