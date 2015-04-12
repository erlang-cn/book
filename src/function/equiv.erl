-module(equiv).

-export([test/0]).


% SNIP BEGIN func-equiv-id
id(X) ->
    X.
% SNIP END


% SNIP BEGIN func-equiv-list
car([H|_]) -> H.
cdr([_|T]) -> T.
cons(H, T) -> [H|T].
% SNIP END


% SNIP BEGIN func-test-equiv-id
test(id) ->
    a = id(a),
    a = id(id(a));
% SNIP END
% SNIP BEGIN func-test-equiv-car
test(car) ->
    a = car([a]),
    a = car([a,b]);
% SNIP END
% SNIP BEGIN func-test-equiv-cdr
test(cdr) ->
    [] = cdr([a]),
    [b] = cdr([a,b]);
% SNIP END
% SNIP BEGIN func-test-equiv-cons
test(cons) ->
    [a] = cons(a, []),
    [a,b] = cons(a, [b]).
% SNIP END


test() ->
    test(id),
    test(car),
    test(cdr),
    test(cons),
    ok.
