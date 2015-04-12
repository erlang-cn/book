-module(exercise).

-export([test/0, check/1]).

% SNIP BEGIN ex-fac
check(fac) ->
    1 = fac:f(1),
    2 = fac:f(2),
    6 = fac:f(3),
    24 = fac:f(4),
    120 = fac:f(5),
    ok;
% SNIP END
% SNIP BEGIN ex-fib
check(fib) ->
    1 = fib:f(1),
    1 = fib:f(2),
    2 = fib:f(3),
    3 = fib:f(4),
    5 = fib:f(5),
    ok.
% SNIP END

test() ->
    check(fac),
    check(fib),
    ok.
