open Notty
open Notty.I
open Terminal
open Notty_markdown

type t = {
  terminal : Terminal.t;
  pref : string
}

let create () = {
  terminal = Terminal.create ~overflow:Continuous;
  pref = "Option 0"
}

let get_text t =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum aliquet turpis a odio tristique, eget dignissim metus laoreet. **Etiam a ultricies est.**
  Praesent pellentesque sem felis, at pulvinar velit ultricies ut. Sed vehicula vulputate odio non mattis. _Mauris vitae facilisis arcu, eget rhoncus dolor._
  Curabitur massa nisi, placerat a arcu vel, rhoncus suscipit purus. Vivamus blandit at tellus quis sodales.

  Proin vestibulum, libero eget luctus efficitur, libero tellus lobortis sem, at bibendum arcu sem sed quam. Pellentesque id tempor metus.\
  Quisque dapibus, dui ut porttitor varius, nunc orci finibus velit, vitae lacinia nunc metus nec lacus. Maecenas posuere tortor mi, quis pretium neque pulvinar venenatis.\
  Nam pellentesque sapien in orci auctor tempor. Mauris nibh nunc, convallis vitae ultricies sed, ultrices id quam. Suspendisse ut dapibus lacus, vulputate sollicitudin massa.

  # Hello
  Mauris mi lorem, dapibus ut ultrices a, dapibus id est. Nulla pharetra, nisl id mattis posuere, lacus purus feugiat enim, ac tincidunt sem dolor eu velit.
  Nullam vel posuere sapien. Maecenas magna dolor, tincidunt a velit sagittis, auctor pretium enim. Integer sollicitudin pellentesque est laoreet consectetur.
  Nunc sed commodo mauris. Donec ante urna, pretium tempus turpis eget, ultricies viverra lacus. Aliquam fermentum risus leo, id rutrum lectus mollis in.
  
  Donec facilisis venenatis turpis, et consectetur justo dignissim a. Morbi dolor magna, dictum vitae nibh vitae, commodo pretium diam."

let create_content t = create_image ~width:(fst (get_dimensions t.terminal)) (get_text t)

let get_controls t = [Control.create (t.pref ^ ".1") 1; Control.create (t.pref ^ ".2") 2; Control.create (t.pref ^ ".3") 3]

let rec main_loop t =
  let controls = get_controls t in
  match tick t.terminal (create_content t) controls with
  | Stop -> ()
  | Continue(t') -> main_loop { t with terminal = t' }
  | Control(t', i) -> main_loop {
    terminal = t';
    pref = t.pref ^ "." ^ (string_of_int i)
  }
  