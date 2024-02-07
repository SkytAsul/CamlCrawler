open Notty
open Notty.I
open Notty.A
open Omd

type line = (attr * string) list

let rec merge_nested_lists list1 list2 =
match list1, list2 with
| _,[] -> list1
| [],_ -> list2
| last1::[], first2::tail2 -> (last1 @ first2) :: tail2
| head1::tail1, _ -> head1 :: (merge_nested_lists tail1 list2)

let rec parse_inline attr inline : (line list) = match inline with
| Text(_, text) -> [[(attr, text)]]
| Strong(_, rest) -> parse_inline (attr ++ st bold) rest
| Emph(_, rest) -> parse_inline (attr ++ st italic) rest
| Concat(_, l) -> parse_inline_list attr l
| _ -> failwith "unsupported text type"
and parse_inline_list attr = function
| [] -> []
| Soft_break(_)::tail -> merge_nested_lists [[(empty, " ")]] (parse_inline_list attr tail)
| Hard_break(_)::tail -> [(empty, "")]::(parse_inline_list attr tail)
| head::tail -> merge_nested_lists (parse_inline attr head) (parse_inline_list attr tail)

let parse_block = function
| Paragraph(_, inline) -> parse_inline A.empty inline
| Heading(_, _, inline) -> merge_nested_lists [[(A.empty, "  ")]] (parse_inline (A.st A.bold) inline)
| _ -> failwith "unsupported text type"

let parse text =
  let rec parse_doc (doc : doc) = match doc with
  | [] -> []
  | a::tail -> (parse_block a) @ ([(empty, "")] :: (parse_doc tail))
  (* maybe better solution for blank line *)
  in parse_doc (of_string text)

let split_long_text width (line : line) : line list =
  let rec aux spare_w = function
  | [] -> []
  | (attr, text)::tail -> let len = String.length text in
    if len <= spare_w then merge_nested_lists [[(attr, text)]] (aux (spare_w - len) tail)
    else begin match String.rindex_from_opt text spare_w ' ' with
    | None -> (* there is no space for one word: we must either postpone it for next line, or split it we are on a blank line *)
      if spare_w < width then aux width ((attr, String.sub text spare_w (len - spare_w)) :: tail)
      else [(attr, String.sub text 0 width)] :: (aux width ((attr, String.sub text width (len - width)) :: tail))
    | Some(last_space) -> (* there is enough space for at least one word *)
      [(attr, String.sub text 0 last_space)] :: (aux width ((attr, String.sub text (last_space + 1) (len - last_space - 1)) :: tail))
    end
in aux width line

let rec line_to_image (line : line) = match line with
| [] -> I.empty
| (attr, text)::tail -> (string attr text) <|> (line_to_image tail)

let create_image ~width text =
  let raw_lines = parse text in
  let cut_lines = List.flatten (List.map (split_long_text width) raw_lines) in
  let images = List.map line_to_image cut_lines in
  List.fold_left (<->) I.empty images