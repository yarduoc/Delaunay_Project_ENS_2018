
## Delaunay Triangulation 

This OCaml Package was develloped for the first project of the Porg1 class. This
OCaml Package allows you to triangulate a set of points using Delaunay
triangulation.

# Build status 

This project is over, final version is released.

# Requirements 

This library was develloped using OCaml 4.07.0. so we recommend to use it with
this version or higher although it might work with older versions.
Graphic and Unix ocaml libraries are also used in this library

# Build Instructions 

Unzip the archive BastideCoudrayDesauw.zip
Run the command sh compile.sh
You can now use the BCD_1.cma and BCD_2.cma archives
You can also run sh demo.sh to have a demonstration of different features.

# Features 

This library is featuring various implementations using Delaunay triangulation.

    Function init_display
    int -> int
    Initialize a graphic window of width and height asked

    Function delaunay
    point set -> triangle set
    Returns the triangulation of the point_set.

    Function delaunay_stepwise
    point set -> unit
    Displays the triangulation of the set on a step by step display

    Function delaunay_morph_set
    morph_point set -> morph_point set -> float -> triangle set
    Returns the triangulation of the intermediary point set between the first
    and the second morph_point set.

    animated_morphprint
    unit -> unit
    Display an animation of the morphing between two different sets.

    it also implements different types :
     - point
     - morph_point
     - triangle

    with their constructors

    make_point
    float -> float -> point

    make_morph_point
    float -> float -> int -> morph_point

    make_triangle
    point -> point -> point -> triangle


# Code Example 

You can find a code example on the demo.sh file

# Credits 

This project was develloped by Paul Bastide, Alex Coudray and Lauric Dessauw.
The Suject of this project was given by Luc Bougé, Teacher at the ENS Rennes
and Timothée Haudebourg.
The project github at https://github.com/yarduoc/Delaunay_Project_ENS_2018
