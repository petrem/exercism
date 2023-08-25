% kb1
woman(mia).
woman(jody).
woman(yolanda).
playsAirGuitar(jody).
party.

% kb2
happy(yolanda).
listens2Music(mia).
listens2Music(yolanda):-  happy(yolanda).
playsAirGuitar(mia):-  listens2Music(mia).
playsAirGuitar(yolanda):-  listens2Music(yolanda).

% kb4
loves(vincent,mia).
loves(marsellus,mia).
loves(pumpkin,honey_bunny).
loves(honey_bunny,pumpkin).


% kb5
jealous(X,Y):-  loves(X,Z),  loves(Y,Z).

% ex 1.4

%% Butch is a killer.
%% Mia and Marsellus are married.
%% Zed is dead.
%% Marsellus kills everyone who gives Mia a footmassage.
%% Mia loves everyone who is a good dancer.
%% Jules eats anything that is nutritious or tasty.
killer(butch).
married(mia, marsellus).
isDead(zed).
kills(marsellus, X):- givesFootmassage(X, mia).
loves(mia, X):- goodDancer(X).
eats(jules, X):- nutritious(X) ; tasty(X).
nuts.
apple.
muffin.
nutritious(nuts).
nutritious(muffin).
tasty(muffin).
tasty(mia).

% ex 1.5
wizard(ron).
hasWand(harry).
quidditchPlayer(harry).
wizard(X):-  hasBroom(X),  hasWand(X).
hasBroom(X):-  quidditchPlayer(X).

% vertical and horizontal
vertical(line(point(X,Y),point(X,Z))).
horizontal(line(point(X,Y),point(Z,Y))).

% ex 2.2

house_elf(dobby).
witch(hermione).
witch('McGonagall').
witch(rita_skeeter).
magic(X):-  house_elf(X).
magic(X):-  wizard(X).
magic(X):-  witch(X).

% ex 2.3

word(determiner,a).
word(determiner,every).
word(noun,criminal).
word(noun,'big kahuna burger').
word(verb,eats).
word(verb,likes).

sentence(Word1,Word2,Word3,Word4,Word5):-
      word(determiner,Word1),
      word(noun,Word2),
      word(verb,Word3),
      word(determiner,Word4),
      word(noun,Word5).

% ex 2.4

word(astante,  a,s,t,a,n,t,e).
word(astoria,  a,s,t,o,r,i,a).
word(baratto,  b,a,r,a,t,t,o).
word(cobalto,  c,o,b,a,l,t,o).
word(pistola,  p,i,s,t,o,l,a).
word(statale,  s,t,a,t,a,l,e).


crossword(V1,V2,V3,H1,H2,H3):-
    intersect2(V1, H1, H2, H3),
    intersect4(V2, H1, H2, H3),
    intersect6(V3, H1, H2, H3),
    V1 \= V2,
    V2 \= V3,
    V1 \= V3,
    H1 \= H2,
    H1 \= H3,
    H2 \= H3,
    V1 \= H1,
    V1 \= H2,
    V1 \= H3.


pos2(W, L):- word(W, _, L, _, _, _, _, _).
pos4(W, L):- word(W, _, _, _, L, _, _, _).
pos6(W, L):- word(W, _, _, _, _, _, L, _).


intersect2(V, H1, H2, H3):-
    pos2(V, L2), pos2(H1, L2),
    pos4(V, L4), pos2(H2, L4),
    pos6(V, L6), pos2(H3, L6).

intersect4(V, H1, H2, H3):-
    pos2(V, L2), pos4(H1, L2),
    pos4(V, L4), pos4(H2, L4),
    pos6(V, L6), pos4(H3, L6).

intersect6(V, H1, H2, H3):-
    pos2(V, L2), pos6(H1, L2),
    pos4(V, L4), pos6(H2, L4),
    pos6(V, L6), pos6(H3, L6).


% 2.4

f(a).
f(b).
   
g(a).
g(b).
   
h(b).
   
k(X):-
    f(X),
    g(X),
    h(X). 

% 3

divides(X, Y):-
    X =:= 1;
    X =:= Y;
    Y > X, divides(X, Y - X).

bottom :- bottom.


child(anne,bridget).
child(bridget,caroline).
child(caroline,donna).
child(donna,emily).

%% transitive(Rel2):-
%%     tr_helper(Rel2, X, Y, Z):- Rel2(X, Y).
%% tr_helper(Rel2, X, Y, Z):-
%%     Rel2(X, Z), Rel2(Z, Y).

descendent(X, Y):- child(X, Y).
descendent(X, Y):-
    child(X, Z),
    descendent(Z, Y).


%%
numeral(0).
numeral(succ(X)):- numeral(X).

add(N, 0, N).
add(0, N, N).
add(N1, succ(N2), succ(N3)):-
    add(N1, N2, N3).

%% 3.3

descend2(X,Y)  :-  child(X,Y).
descend2(X,Y)  :-  descend2(X,Z),
                   descend2(Z,Y). 

directlyIn(natasha, irina).
directlyIn(olga, natasha).
directlyIn(katarina, olga).
isIn(X, Y):-
    directlyIn(X, Y).
isIn(X, Y):-
    directlyIn(X, Z), isIn(Z, Y).

greater_than(0, 0):- false.
greater_than(succ(X), 0).
greater_than(succ(X), succ(Y)):- greater_than(X, Y).

swap(leaf(X), leaf(X)).
swap(tree(L, R), tree(Rswapped, Lswapped)):-
    swap(L, Lswapped),
    swap(R, Rswapped). 
