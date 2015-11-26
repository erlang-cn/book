-module(func_lambda).
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
;
% SNIP BEGIN func-label
call({fn, label}, [X,Y], Env) ->
    {Y1, Env1} = apply(Y, Env),
    {Y1, [{X, Y1}|Env1]}
%- SNIP END
;
% SNIP BEGIN func-list
call({fn, car}, [X], Env) ->
    {{data, [H|_]}, Env1} = apply(X, Env),
    {{data, H}, Env1};
call({fn, cdr}, [X], Env) ->
    {{data, [_|T]}, Env1} = apply(X, Env),
    {{data, T}, Env1};
call({fn, cons}, [X,Y], Env) ->
    {[{data,X1}, {data,Y1}], Env1} =
        eval_list([X,Y], Env),
    {{data, [X1|Y1]}, Env1}
%- SNIP END
;
% SNIP BEGIN func-cond
call({fn, 'cond'}, [[P,E]|T], Env) ->
    {{data, R}, Env1} = apply(P, Env),
    case R of
        false ->
            apply(['cond'|T], Env1);
        true ->
            apply(E, Env1)
    end
%- SNIP END
;
% SNIP BEGIN func-atom
call({fn, atom}, [X], Env) ->
    {{data, X1}, Env1} = apply(X, Env),
    {{data, is_atom(X1)}, Env1};
% SNIP END
% SNIP BEGIN func-eq
call({fn, eq}, [X,Y], Env) ->
    {[{data,X1}, {data,Y1}], Env1} =
        eval_list([X,Y], Env),
    {{data, eq(X1,Y1)}, Env1}
%- SNIP END
;
% SNIP BEGIN func-lambda
call({fn, lambda}, [P,E], Env) ->
    {{lambda, {P,E}}, Env};
call({lambda, {P,E}}, Args, Env) ->
    {Args1, Env1} = eval_list(Args, Env),
    {V, _} = apply(E, append(zip(P, Args1), Env1)),
    {V, Env1}
% SNIP END
.


% SNIP BEGIN func-env-quote
new_env() ->
    [{quote, {fn, quote}}
%- SNIP END
,
% SNIP BEGIN func-env-label
     {label, {fn, label}}
%- SNIP END
,
% SNIP BEGIN func-env-list
     {car,   {fn, car}},
     {cdr,   {fn, cdr}},
     {cons,  {fn, cons}}
%- SNIP END
,
% SNIP BEGIN func-env-cond
     {'cond',{fn, 'cond'}}
%- SNIP END
,
% SNIP BEGIN func-env-predicate
     {atom,  {fn, atom}},
     {eq,    {fn, eq}}
%- SNIP END
,
% SNIP BEGIN func-env-lambda
     {lambda,{fn, lambda}}
%- SNIP END
].


% SNIP BEGIN func-eval-list
eval_list([], Env) ->
    {[], Env};
eval_list([H|T], Env) ->
    {VH, Env1} = apply(H, Env),
    {VT, Env2} = eval_list(T, Env1),
    {[VH|VT], Env2}.
% SNIP END


% SNIP BEGIN func-helper-eq
eq([], []) ->
    true;
eq(X, Y)
  when is_atom(X), is_atom(Y) ->
    X =:= Y;
eq(_, _) ->
    false.
% SNIP END


% SNIP BEGIN func-helper-append
append([], L2) ->
    L2;
append([H|T], L2) ->
    [H|append(T, L2)].
% SNIP END


% SNIP BEGIN func-helper-zip
zip([], []) ->
    [];
zip([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip(T1, T2)].
% SNIP END


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
;
% SNIP BEGIN func-test-label
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
                  ], new_env())
%- SNIP END
;
% SNIP BEGIN func-test-list
test(list) ->
    {{data, a}, _} =
        apply([car, [quote, [a]]], new_env()),
    {{data, a}, _} =
        apply([car, [quote, [a,b]]], new_env()),
    {{data, []}, _} =
        apply([cdr, [quote, [a]]], new_env()),
    {{data, [b]}, _} =
        apply([cdr, [quote, [a,b]]], new_env()),
    {{data, [a]}, _} =
        apply([cons, [quote, a], [quote, []]],
              new_env()),
    {{data, [a,b]}, _} =
        apply([cons, [quote, a], [quote, [b]]],
              new_env())
%- SNIP END
;
% SNIP BEGIN func-test-cond
test('cond') ->
    {{data, a}, _} =
        apply(['cond',
               [[quote, true],  [quote, a]],
               [[quote, false], [quote, b]],
               [[quote, false], [quote, c]]
              ], new_env()),
    {{data, b}, _} =
        apply(['cond',
               [[quote, false], [quote, a]],
               [[quote, true],  [quote, b]],
               [[quote, false], [quote, c]]
              ], new_env()),
    {{data, c}, _} =
        apply(['cond',
               [[quote, false], [quote, a]],
               [[quote, false], [quote, b]],
               [[quote, true],  [quote, c]]
              ], new_env())
%- SNIP END
;
% SNIP BEGIN func-test-atom
test(atom) ->
    {{data, true}, _} =
        apply([atom, [quote, a]], new_env()),
    {{data, false}, _} =
        apply([atom, [quote, [a,b,c]]], new_env());
% SNIP END
% SNIP BEGIN func-test-eq
test(eq) ->
    {{data, true}, _} =
        apply([eq, [quote, []], [quote, []]],
              new_env()),
    {{data, true}, _} =
        apply([eq, [quote, a], [quote, a]],
              new_env()),
    {{data, false}, _} =
        apply([eq, [quote, []], [quote, a]],
              new_env()),
    {{data, false}, _} =
        apply([eq, [quote, a], [quote, []]],
              new_env()),
    {{data, false}, _} =
        apply([eq, [quote, [a]], [quote, [b]]],
              new_env()),
    {{data, false}, _} =
        apply([eq, [quote, [a,b,c]], [quote, [a,b,c]]],
              new_env())
%- SNIP END
;
% SNIP BEGIN func-test-lambda
test(lambda) ->
    {{data, a}, _} =
        apply([[lambda, [x], x],
               [quote, a]], new_env()),
    {{data, a}, _} =
        apply([[lambda, [x], [car, x]],
               [quote, [a,b,c]]], new_env()),
    {{data, a}, _} =
        apply([[lambda, [x], [car, [car, x]]],
               [quote, [[a]]]], new_env()),
    {{data, true}, _} =
        apply([[lambda, [x, y], [eq, x, y]],
               [quote, a],
               [quote, a]], new_env()),
    {{data, a}, _} =
        apply([[lambda, [x, x], x],
               [quote, a],
               [quote, b]], new_env())
%- SNIP END
.


test() ->
    test(subst),
    test(quote),
    test(label),
    test(list),
    test('cond'),
    test(atom),
    test(eq),
    test(lambda),
    ok.
