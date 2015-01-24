-module(equiv).

-export([test/0]).


id(X) ->
    X.


car([H|_]) -> H.
cdr([_|T]) -> T.
cons(H, T) -> [H|T].


test(id) ->
    a = id(a),
    a = id(id(a));
test(car) ->
    a = car([a]),
    a = car([a,b]);
test(cdr) ->
    [] = cdr([a]),
    [b] = cdr([a,b]);
test(cons) ->
    [a] = cons(a, []),
    [a,b] = cons(a, [b]).


test() ->
    test(id),
    test(car),
    test(cdr),
    test(cons),
    ok.
