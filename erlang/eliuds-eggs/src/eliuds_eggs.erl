-module(eliuds_eggs).

-export([egg_count/1]).

egg_count(0) -> 0;
egg_count(_Number) -> _Number band 1 + egg_count(_Number bsr 1).
