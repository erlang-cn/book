-module(func2).
% SNIP BEGIN func-no-auto-import
-compile({no_auto_import, [apply/2]}).
% SNIP END

-export([test/0]).


% SNIP BEGIN func-subst
subst(_, []) ->
    none;
subst(K, [{K, V}|_]) ->
    {ok, V};
subst(K, [_|T]) ->
    subst(K, T).
% SNIP END


% SNIP BEGIN func-apply
apply([H|T], Env) ->
    {Fun, Env1} = apply(H, Env),
    call(Fun, T, Env1);
apply(Expr, Env) ->
    true = is_atom(Expr),
    {ok, Value} = subst(Expr, Env),
    {Value, Env}.
% SNIP END


% SNIP BEGIN func-quote
call({fn, quote}, [X], Env) ->
    {{data, X}, Env}
%- SNIP END
.


% SNIP BEGIN func-env-quote
new_env() ->
    [{quote, {fn, quote}}
%- SNIP END
].


% SNIP BEGIN func-test-subst
test(subst) ->
    {ok, c} = subst(a, [{b,d},{a,c}]),
    none = subst(a, [{b,d}]),
    {ok, c} = subst(a, [{a,c},{b,d},{a,e}]);
% SNIP END
% SNIP BEGIN func-test-quote1
test(quote) ->
    {{data, a}, _} =
        apply([quote, a], new_env()),
    {{data, [a,b,c]}, _} =
        apply([quote, [a,b,c]], new_env())
%- SNIP END
,
% SNIP BEGIN func-test-quote2
    {{data, [quote, a]}, _} =
        apply([quote, [quote, a]], new_env())
%- SNIP END
.

test() ->
    test(subst),
    test(quote),
    ok.
