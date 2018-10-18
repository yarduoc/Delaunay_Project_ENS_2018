type 'a matrix = 'a array array
val create_matrix_3x3 : unit -> float matrix
val indice : 'a matrix -> int -> int -> 'a
val det_2 : float matrix -> float
val det_3 : float matrix -> float
