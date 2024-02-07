open Terminal
open Notty_markdown
open Story

type t = {
  story: story;
  terminal : Terminal.t;
  choices_made: string list;
  parts_seen: string list;
}

let create story = {
  story;
  terminal = Terminal.create ~overflow:Continuous;
  choices_made = [];
  parts_seen = [story.beginning];
}

let get_active_part t = match t.parts_seen with
| part::_ -> get_part t.story part
| [] -> failwith "Cannot happen; there is at least one part."

let rec is_condition_met t = function
| Not(cond) -> not(is_condition_met t cond)
| And(list) -> List.for_all (is_condition_met t) list
| Or(list) -> List.exists (is_condition_met t) list
| Choice(choice_id) -> List.exists (fun c -> c = choice_id) t.choices_made
| Part(part_id) -> List.exists (fun p -> p = part_id) t.parts_seen

let create_content t = create_image ~width:(fst (get_dimensions t.terminal)) (get_active_part t).part_text

let rec get_choices_controls t = function
| [] -> []
| c::tail -> if match c.condition with None -> true | Some(cond) -> is_condition_met t cond
  then Control.create c.choice_text c.choice_id :: get_choices_controls t tail
else get_choices_controls t tail

let get_controls t = get_choices_controls t (get_part_choices t.story (get_active_part t))

let rec main_loop t =
  try
    let controls = get_controls t in
    match tick t.terminal (create_content t) controls with
    | Stop -> ()
    | Continue(t') -> main_loop { t with terminal = t' }
    | Control(t', choice_id) -> main_loop {
        t with terminal = t';
        choices_made = choice_id::t.choices_made;
        parts_seen = (get_choice t.story choice_id).destination_part :: t.parts_seen
      }
  with e ->
    Terminal.release t.terminal;
    raise e