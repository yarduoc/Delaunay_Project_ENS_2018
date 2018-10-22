val rand_points: int -> float -> float -> Pointtriangle.point_set
val rand_m_points:  int -> float -> float -> Pointtriangle.morph_point_set
val delaunay : Pointtriangle.point_set -> int -> int -> Pointtriangle.triangle_set
val delaunay_stepwise: Pointtriangle.point_set -> int -> int -> unit
val delaunay_morph_set: Pointtriangle.morph_point_set -> Pointtriangle.morph_point_set -> float -> float -> float -> Pointtriangle.triangle_set
val animated_morphprint: unit -> unit
