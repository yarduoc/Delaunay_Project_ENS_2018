(*© Copyright Paul Bastide, Alex Coudray, Lauric Desauw 25/04/2019 CC-BY 4.0*)

type 'a matrix = 'a array array
val create_matrix_3x3 : unit -> float array array
val indice : 'a array array -> int -> int -> 'a
val det_2 : float array array -> float
val det_3 : float array array -> float
