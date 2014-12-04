-module(exercise).

-export([test/0, check/1]).

check(fac) ->
    1 = fac:f(1),
    2 = fac:f(2),
    6 = fac:f(3),
    24 = fac:f(4),
    120 = fac:f(5),
    ok;
check(fib) ->
    1 = fib:f(1),
    1 = fib:f(2),
    2 = fib:f(3),
    3 = fib:f(4),
    5 = fib:f(5),
    ok.

test() ->
    check(fac),
    check(fib),
    ok.
