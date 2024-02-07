type condition =
| Not of condition
| And of condition list
| Or of condition list
| Choice of string
| Part of string

type choice = {
  choice_id: string;
  choice_text: string;
  condition: condition option;
  destination_part: string
}

type part = {
  part_id: string;
  part_text: string;
  part_choices: string list
}

type story = {
  parts: part list;
  choices: choice list;
  beginning: string
}

let get_choice story choice_id = List.find (fun c -> c.choice_id = choice_id) story.choices

let get_part story part_id = List.find (fun c -> c.part_id = part_id) story.parts

let get_part_choices story part = List.map (fun c -> get_choice story c) part.part_choices