(*© Copyright Paul Bastide, Alex Coudray, Lauric Desauw 25/04/2019 CC-BY 4.0*)

val is_counterclockwise : Pointtriangle.point -> Pointtriangle.point -> Pointtriangle.point -> bool
val in_circle : Pointtriangle.triangle -> Pointtriangle.point -> bool
val to_modify_tri : Pointtriangle.triangle_set -> Pointtriangle.point -> Pointtriangle.triangle_set
