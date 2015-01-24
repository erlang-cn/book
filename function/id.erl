-module(id).
-compile({no_auto_import, [apply/2]}).

-export([test/0, apply/2, new_env/0]).


subst(_, []) ->
    none;
subst(K, [{K, V}|_]) ->
    {ok, V};
subst(K, [_|T]) ->
    subst(K, T).


apply([H|T], Env) ->
    {Fun, Env1} = apply(H, Env),
    call(Fun, T, Env1);
apply(Expr, Env) ->
    true = is_atom(Expr),
    {ok, Value} = subst(Expr, Env),
    {Value, Env}.


call({fn, quote}, [X], Env) ->
    {{data, X}, Env};
call({fn, id}, [X], Env) ->
    apply(X, Env).


new_env() ->
    [{quote, {fn, quote}},
     {id,    {fn, id}}].


test(subst) ->
    {ok, c} = subst(a, [{b,d},{a,c}]),
    none = subst(a, [{b,d}]),
    {ok, c} = subst(a, [{a,c},{b,d},{a,e}]);
test(quote) ->
    {{data, a}, _} =
        apply([quote, a], new_env()),
    {{data, [quote, a]}, _} =
        apply([quote, [quote, a]], new_env()),
    {{data, [quote, a]}, _} =
        apply([quote, [quote, a]], new_env()).


test() ->
    test(subst),
    test(quote),
    ok.
