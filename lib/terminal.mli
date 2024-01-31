module Control : sig
    type 'a t
    
    val create : string -> 'a -> 'a t

    val get_label : 'a t -> string
end

type t

type 'a event_result = | Stop | Continue of t | Control of t * 'a

val create : unit -> t

val get_dimensions : t -> int * int

val handle_events : t -> 'a Control.t list -> 'a event_result

val draw_image : t -> Notty.I.t -> 'a Control.t list -> unit