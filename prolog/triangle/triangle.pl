triangle(Side1, Side2, Side3, Type):-
    isTriangle(Side1, Side2, Side3),
    checkTriangleType(Side1, Side2, Side3, Type).

isTriangle(Side1, Side2, Side3):-
    Side1 > 0,
    Side2 > 0,
    Side3 > 0,
    Side1 + Side2 > Side3,
    Side1 + Side3 > Side2,
    Side2 + Side3 > Side1.

isIsosceles(Side1, Side2, Side3):-
    Side1 =:= Side2;
    Side1 =:= Side3;
    Side2 =:= Side3.

isEquilateral(Side1, Side2, Side3):-
    Side1 =:= Side2, Side2 =:= Side3.

isScalene(Side1, Side2, Side3):-
    Side1 =\= Side2,
    Side1 =\= Side3,
    Side2 =\= Side3.

checkTriangleType(Side1, Side2, Side3, Type):-
    Type="isosceles", isIsosceles(Side1, Side2, Side3);
    Type="equilateral", isEquilateral(Side1, Side2, Side3);
    Type="scalene", isScalene(Side1, Side2, Side3).

