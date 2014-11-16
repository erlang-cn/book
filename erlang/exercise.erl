-module(exercise).

-export([test/0, check/1]).

check(fac) ->
    1 = fac:f(1),
    2 = fac:f(2),
    6 = fac:f(3),
    24 = fac:f(4),
    120 = fac:f(5),
    ok.


test(Module) ->
    {ok, Module} = c:c(Module),
    check(Module).

test() ->
    test(fac),
    ok.
