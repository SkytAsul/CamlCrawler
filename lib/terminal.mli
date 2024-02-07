module Control : sig
    type 'a t
    
    val create : string -> 'a -> 'a t

    val get_label : 'a t -> string
end

type t

type 'a control = 'a Control.t

type 'a event_result = | Stop | Continue of t | Control of t * 'a

type overflow = | Continuous | Paginated

val create : overflow:overflow -> t

val release : t -> unit

val get_dimensions : t -> int * int

val tick : t -> Notty.image -> 'a control list -> 'a event_result