(*© Copyright Paul Bastide, Alex Coudray, Lauric Desauw 25/04/2019 CC-BY 4.0*)
type 'a set
val empty : unit -> 'a set
val cons : 'a set -> 'a -> 'a set
val car : 'a set -> 'a
val cdr : 'a set -> 'a set
val concat : 'a set -> 'a set -> 'a set
val length : 'a set -> int
val find : 'a set -> 'a -> bool
val suppress : 'a set ref -> 'a -> unit
val iter : ('a -> 'b) -> 'a set -> unit
val is_empty : 'a set -> bool
val copy : 'a -> 'a
val sort : ('a -> 'a -> bool) -> 'a set -> 'a set
val ord_insert : ('a -> 'a -> bool) -> 'a set -> 'a -> 'a set
val del : 'a set -> 'a -> 'a set
