triangle(Side1, Side2, Side3, Type):-
    is_triangle(Side1, Side2, Side3),
    triangle_type(Side1, Side2, Side3, Type).

is_triangle(Side1, Side2, Side3):-
    Side1 > 0,
    Side2 > 0,
    Side3 > 0,
    Side1 + Side2 > Side3,
    Side1 + Side3 > Side2,
    Side2 + Side3 > Side1.

triangle_type(Side1, Side2, Side3, "isosceles"):-
    Side1 =:= Side2;
    Side1 =:= Side3;
    Side2 =:= Side3.

triangle_type(Side1, Side2, Side3, "equilateral"):-
    Side1 =:= Side2, Side2 =:= Side3.

triangle_type(Side1, Side2, Side3, "scalene"):-
    Side1 =\= Side2,
    Side1 =\= Side3,
    Side2 =\= Side3.
