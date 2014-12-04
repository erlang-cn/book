-module(id).
-compile({no_auto_import, [apply/2]}).

-export([apply/2, new_env/0]).


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
call({fn, id}, [X], Env) ->
    apply(X, Env).


new_env() ->
    [{quote, {fn, quote}},
     {id,    {fn, id}}].
