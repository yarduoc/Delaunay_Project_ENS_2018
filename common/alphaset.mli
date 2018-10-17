type 'a set = 'a list
val empty : unit -> 'a list
val cons : 'a list -> 'a -> 'a list
val car : 'a list -> 'a
val cdr : 'a list -> 'a list
val length : 'a list -> int
val find : 'a list -> 'a -> bool
val suppress : 'a list ref -> 'a -> unit
val iter : ('a -> 'b) -> 'a list -> unit
val is_empty : 'a list -> bool
