% SNIP BEGIN ans-fib
-module(fib).

-export([f/1]).

f(1) ->
    1;
f(2) ->
    1;
f(N)
  when N > 2->
    f(N-2) + f(N-1).
% SNIP END
