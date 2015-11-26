-module(exercise).

-export([test/0, check/1]).


% SNIP BEGIN ex-environ
check(environ) ->
    Env = environ:empty(),
    false = environ:is_bound(a, Env),
    Env1 = environ:bind(a, b, Env),
    true = environ:is_bound(a, Env1),
    b = environ:get_value(a, Env1),
    ok;
% SNIP END
% SNIP BEGIN ex-sexpr
check(sexpr) ->
    true = sexpr:is_sexpr(a),
    false = sexpr:is_sexpr(1),
    true = sexpr:is_sexpr([]),
    true = sexpr:is_sexpr([a]),
    true = sexpr:is_sexpr([a, b, c]),
    false = sexpr:is_sexpr([1]),
    false = sexpr:is_sexpr([{a,a}]),
    ok;
% SNIP END
% SNIP BEGIN ex-id
check(id) ->
    {{data, a}, _} =
        func_id:apply([id, [quote, a]], func_id:new_env()),
    {{data, a}, _} =
        func_id:apply([id, [id, [quote, a]]], func_id:new_env()),
    ok;
% SNIP END
% SNIP BEGIN ex-defun
check(defun) ->
    {[_,{data, a}], _} =
        defun:eval_list(
          [[defun, caar, [x], [car, [car, x]]],
           [caar, [quote, [[a]]]]], defun:new_env()).
% SNIP END


test() ->
    check(environ),
    check(sexpr),
    check(id),
    check(defun),
    ok.
