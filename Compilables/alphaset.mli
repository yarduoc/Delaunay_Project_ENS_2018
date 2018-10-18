type 'a set
val empty : unit -> 'a set
val cons : 'a set -> 'a -> 'a set
val car : 'a set -> 'a
val cdr : 'a set -> 'a set
val length : 'a set -> int
val find : 'a set -> 'a -> bool
val suppress : 'a set ref -> 'a -> unit
val iter : ('a -> 'b) -> 'a set -> unit
val is_empty : 'a set -> bool
val copy : 'a set -> 'a set
