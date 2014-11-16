-module(fac).

-export([f/1]).

f(1) ->
    1;
f(N)
  when N > 1->
    N * f(N-1).
