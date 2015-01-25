type t
val empty : t
val mem : string -> t -> bool
val add : string -> t -> t
val remove : string -> t -> t
val iter : f:(string -> unit) -> t -> unit
