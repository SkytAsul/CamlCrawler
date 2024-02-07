open Story

let assoc_string key assoc = match List.assoc key assoc with
| `String(s) -> s
| _ -> failwith "Syntax error"

let assoc_assoc_list key assoc = match List.assoc key assoc with
| `O(list) -> list
| _ -> failwith "Syntax error"

let assoc_list key assoc = match List.assoc key assoc with
| `A(list) -> list
| _ -> failwith "Syntax error"

let rec parse_condition value = match value with
| `O(["not", inner]) -> Not (parse_condition inner)
| `O(["and", `A(list)]) -> And (List.map parse_condition list)
| `O(["or", `A(list)]) -> Or (List.map parse_condition list)
| `O(["choice", `String(choice)]) -> Choice (choice)
| `O(["part", `String(part)]) -> Part (part)
| _ -> failwith "Syntax error"

let parse_choice choice_id assoc =
  let choice_text = assoc_string "text" assoc in
  let destination_part = assoc_string "destination" assoc in
  let condition = match List.assoc_opt "condition" assoc with
  | None -> None
  | Some(value) -> Some (parse_condition value)
  in { choice_id; choice_text; condition; destination_part }

let rec parse_choices = function
| (k, `O(choice_assoc))::tail -> parse_choice k choice_assoc :: parse_choices tail
| [] -> []
| _ -> failwith "Syntax error"

let parse_part part_id assoc =
  let part_text = assoc_string "text" assoc in
  let part_choices = List.map (function `String(s) -> s | _ -> failwith "Syntax error") (assoc_list "choices" assoc) in
  { part_id; part_text; part_choices }

let rec parse_parts = function
| (k, `O(part_assoc))::tail -> parse_part k part_assoc :: parse_parts tail
| [] -> []
| _ -> failwith "Syntax error"

let parse string = match Yaml.of_string_exn string with
| `O assoc ->
  let beginning = assoc_string "beginning" assoc in
  let parts = parse_parts (assoc_assoc_list "parts" assoc) in
  let choices = parse_choices (assoc_assoc_list "choices" assoc) in
  { beginning; parts; choices }
| _ -> failwith "Syntax error"

let parse_file path =
  let ic = Stdlib.open_in path in
  try
    let contents = Stdlib.really_input_string ic (Stdlib.in_channel_length ic) in
    close_in ic;
    parse contents
  with e ->
    close_in_noerr ic;
    raise e