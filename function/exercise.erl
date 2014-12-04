-module(exercise).

-export([test/0, check/1]).

check(environ) ->
    Env = environ:empty(),
    false = environ:is_bound(a, Env),
    Env1 = environ:bind(a, b, Env),
    true = environ:is_bound(a, Env1),
    b = environ:get_value(a, Env1),
    ok;
check(sexpr) ->
    true = sexpr:is_sexpr(a),
    false = sexpr:is_sexpr(1),
    true = sexpr:is_sexpr([]),
    true = sexpr:is_sexpr([a]),
    true = sexpr:is_sexpr([a, b, c]),
    false = sexpr:is_sexpr([1]),
    false = sexpr:is_sexpr([{a,a}]),
    ok;
check(id) ->
    {{data, a}, _} =
        id:apply([id, [quote, a]], id:new_env()),
    {{data, a}, _} =
        id:apply([id, [id, [quote, a]]], id:new_env()),
    ok.


test() ->
    check(environ),
    check(sexpr),
    check(id),
    ok.
