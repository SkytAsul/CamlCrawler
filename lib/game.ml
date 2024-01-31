open Notty
open Notty.I
open Terminal
open Notty_markdown

type t = {
  terminal : Terminal.t;
  pref : string
}

let create () = {
  terminal = Terminal.create ();
  pref = "Option 0"
}

let get_text t =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum aliquet turpis a odio tristique, eget dignissim metus laoreet. **Etiam a ultricies est.**
  Praesent pellentesque sem felis, at pulvinar velit ultricies ut. Sed vehicula vulputate odio non mattis. _Mauris vitae facilisis arcu, eget rhoncus dolor._"

let create_content t = create_image ~width:(fst (get_dimensions t.terminal)) (get_text t)

let get_controls t = [Control.create (t.pref ^ ".1") 1; Control.create (t.pref ^ ".2") 2; Control.create (t.pref ^ ".3") 3]

let rec main_loop t =
  let controls = get_controls t in
  draw_image t.terminal (create_content t) controls;
  match handle_events t.terminal controls with
  | Stop -> ()
  | Continue(t') -> main_loop { t with terminal = t' }
  | Control(t', i) -> main_loop {
    terminal = t';
    pref = t.pref ^ "." ^ (string_of_int i)
  }
  