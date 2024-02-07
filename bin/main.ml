open CamlCrawler

(* let () = Terminal_example.main_loop (Terminal_example.create ()) *)

let story = Story_parser.parse_file "story.yml"
let game = Game.create story
let () = Game.main_loop game