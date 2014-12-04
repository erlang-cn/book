-module(func3).
-compile({no_auto_import, [apply/2]}).

-export([test/0]).


subst(_, []) ->
    none;
subst(K, [{K, V}|_]) ->
    {ok, V};
subst(K, [_|T]) ->
    subst(K, T).


apply(Expr, Env)
  when is_atom(Expr) ->
    {ok, Value} = subst(Expr, Env),
    {Value, Env};
apply([H|T], Env) ->
    {Fun, Env1} = apply(H, Env),
    call(Fun, T, Env1).


call({fn, quote}, [X], Env) ->
    {{data, X}, Env};
call({fn, label}, [X,Y], Env) ->
    {Y1, Env1} = apply(Y, Env),
    {Y1, [{X, Y1}|Env1]}.


new_env() ->
    [{quote, {fn, quote}},
     {label, {fn, label}}].


eval_list([], Env) ->
    {[], Env};
eval_list([H|T], Env) ->
    {VH, Env1} = apply(H, Env),
    {VT, Env2} = eval_list(T, Env1),
    {[VH|VT], Env2}.


test(quote) ->
    {{data, a}, _} =
        apply([quote, a], new_env()),
    {{data, [a,b,c]}, _} =
        apply([quote, [a,b,c]], new_env());
test(label) ->
    {[{data, a}, {data, a}], _} =
        eval_list([[label, x, [quote, a]],
                   x
                  ], new_env()),
    {[{data, b}, {data, b}], _} =
        eval_list([[label, x, [quote, b]],
                   x
                  ], new_env()),
    {[{data, a}, {data, b}, {data, b}], _} =
        eval_list([[label, x, [quote, a]],
                   [label, x, [quote, b]],
                   x
                  ], new_env()).

test() ->
    test(quote),
    test(label),
    ok.
