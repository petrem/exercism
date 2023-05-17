%! create(+DimTuple)
%
% The create/1 predicate succeeds if the DimTuple contains valid chessboard 
% dimensions, e.g. (0,0) or (2,4).
create((DimX, DimY)) :-
    validTuple((DimX, DimY)).

%! attack(+FromTuple, +ToTuple)
%
% The attack/2 predicate succeeds if a queen positioned on ToTuple is 
% vulnerable to an attack by another queen positioned on FromTuple.
attack((FromX, FromY), (ToX, ToY)):-
    FromX = ToX;
    FromY = ToY;
    abs(FromX - ToX) =:= abs(FromY - ToY).

validCoord(0).
validCoord(1).
validCoord(2).
validCoord(3).
validCoord(4).
validCoord(5).
validCoord(6).
validCoord(7).
validTuple((X, Y)):- validCoord(X), validCoord(Y).

%% Yes, I'm sure this can be improved. I'm yet at the early chapters of Prolog :)
