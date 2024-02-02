open Notty
open Notty.I
open Notty_unix
(*
NB: opening this many modules do not seem to be encouraged, but as we need to
use the infix operators of Notty.I, we have to open it.
*)

module Control = struct
  type 'a t = {
    label : string;
    data : 'a
  }

  let create label data = { label; data }

  let get_label t = t.label
end

type 'a control = 'a Control.t

type overflow = | Continuous | Paginated

type t = {
  term : Term.t;
  overflow: overflow;
  overflow_skipped: int;
  selected : int
}

type 'a event_result = | Stop | Continue of t | Control of t * 'a

let create ~overflow = {term = Term.create (); selected = 0; overflow; overflow_skipped = 0}

let get_dimensions t = Term.size t.term

let rec create_controls_image t i (controls : 'a Control.t list) = match controls with
| [] -> I.empty
| head::tail ->
  let image = if i == t.selected then string (A.st A.bold) ("> " ^ head.label) else string A.empty ("  " ^ head.label) in
  let next = create_controls_image t (i+1) tail in
  if next == I.empty then image else image <-> next

let create_ribbon t = I.char (A.bg (A.gray 12)) ' ' (fst (Term.size t.term)) 1
(* let create_ribbon t content controls =
  let available_h = snd (Term.size t.term) - List.length controls - 1 in
  match t.overflow with
  | Continuous -> empty
  | Paginated -> let page_count = height content / available_h in 0 *)
(* [ ...           Page 1 of 5   ] *)
(* [ ...         Line 16 of 40   ] *)

let draw_image t content controls =
  let controls_img = create_controls_image t 0 controls in
  let ribbon_img = create_ribbon t in
  let (_, h) = Term.size t.term in
  let available_h = h - height controls_img - height ribbon_img in
  let cropped_content = match t.overflow with
  | Continuous -> vcrop t.overflow_skipped 0 content
  | Paginated -> vcrop (t.overflow_skipped * available_h) 0 content
  in
  let cropped_content = if height cropped_content <= available_h then cropped_content else vcrop 0 (height cropped_content - available_h) cropped_content in
  let space = void 1 (available_h - height cropped_content) in
  let full_image = cropped_content <-> space <-> ribbon_img <-> controls_img in
  Term.image t.term full_image

let handle_events t (controls : 'a Control.t list) = match Term.event t.term with
| `End | `Key(`Escape, []) -> Stop
| `Key(`Arrow(`Down), []) -> Continue(if t.selected == List.length controls - 1 then t else { t with selected = t.selected + 1 })
| `Key(`Arrow(`Up), []) -> Continue(if t.selected == 0 then t else { t with selected = t.selected - 1 })
| `Key(`Enter, []) -> Control({ t with selected = 0; overflow_skipped = 0 }, (List.nth controls t.selected).data)
| `Key(`Page(`Down), []) | `Mouse(`Press(`Scroll(`Down)), _, []) -> Continue({ t with overflow_skipped = t.overflow_skipped + 1 })
| `Key(`Page(`Up), []) | `Mouse(`Press(`Scroll(`Up)), _, []) -> Continue({ t with overflow_skipped = t.overflow_skipped - 1 })
| _ -> Continue(t)

let tick t content controls =
  draw_image t content controls;
  handle_events t controls;