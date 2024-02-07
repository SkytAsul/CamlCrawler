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

let release t = Term.release t.term

let get_dimensions t = Term.size t.term

let rec create_controls_image t i (controls : 'a Control.t list) = match controls with
| [] -> I.empty
| head::tail ->
  let image = if i == t.selected then string (A.st A.bold) ("> " ^ head.label) else string A.empty ("  " ^ head.label) in
  let next = create_controls_image t (i+1) tail in
  if next == I.empty then image else image <-> next

let create_ribbon t content_h available_h =
  let (w, _) = Term.size t.term in
  let ribbon_text = match t.overflow with
  | Paginated -> let page_count = Float.to_int (Float.ceil (Float.of_int content_h /. Float.of_int(available_h))) in
    "Page " ^ string_of_int (t.overflow_skipped + 1) ^ " of " ^ string_of_int page_count
  | Continuous -> let last_line = min (t.overflow_skipped + available_h) content_h in
    "Line " ^ string_of_int last_line ^ " of " ^ string_of_int content_h
  in let ribbon = I.string (A.bg (A.gray 5)) ("   " ^ ribbon_text ^ "   ") in
  I.void (w - width ribbon) 1 <|> ribbon

let draw_image t content controls =
  let (_, h) = Term.size t.term in
  let controls_img = create_controls_image t 0 controls in
  let available_h = h - height controls_img - 1 in
  let ribbon_img = create_ribbon t (height content) available_h in
  let cropped_content = match t.overflow with
  | Continuous -> vcrop t.overflow_skipped 0 content
  | Paginated -> vcrop (t.overflow_skipped * available_h) 0 content
  in
  let cropped_content = if height cropped_content <= available_h then cropped_content else vcrop 0 (height cropped_content - available_h) cropped_content in
  let space = void 1 (available_h - height cropped_content) in
  let full_image = cropped_content <-> space <-> ribbon_img <-> controls_img in
  Term.image t.term full_image

let handle_events t content (controls : 'a Control.t list) = match Term.event t.term with
| `End | `Key(`Escape, []) -> Stop
| `Key(`Arrow(`Down), []) -> Continue(if t.selected == List.length controls - 1 then t else { t with selected = t.selected + 1 })
| `Key(`Arrow(`Up), []) -> Continue(if t.selected == 0 then t else { t with selected = t.selected - 1 })
| `Key(`Enter, []) -> Control({ t with selected = 0; overflow_skipped = 0 }, (List.nth controls t.selected).data)
| `Key(`Page(`Down), []) | `Mouse(`Press(`Scroll(`Down)), _, []) ->
  let available_h = snd (Term.size t.term) - (List.length controls) - 1 in
  let max_overflow = match t.overflow with
  | Paginated -> Float.to_int (Float.ceil (Float.of_int (height content) /. Float.of_int(available_h)))
  | Continuous -> height content - 1
  in if t.overflow_skipped >= max_overflow - 1 then Continue (t) else Continue ({ t with overflow_skipped = t.overflow_skipped + 1 })
| `Key(`Page(`Up), []) | `Mouse(`Press(`Scroll(`Up)), _, []) ->
  let min_overflow = match t.overflow with
  | Paginated -> 0
  | Continuous -> let available_h = snd (Term.size t.term) - (List.length controls) - 1 in -available_h + 1
  in if t.overflow_skipped <= min_overflow then Continue (t) else Continue({ t with overflow_skipped = t.overflow_skipped - 1 })
(* Mouse click on option ? *)
| _ -> Continue(t)

let tick t content controls =
  draw_image t content controls;
  handle_events t content controls;