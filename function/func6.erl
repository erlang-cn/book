-module(func6).
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
    {Y1, [{X, Y1}|Env1]};
call({fn, car}, [X], Env) ->
    {{data, [H|_]}, Env1} = apply(X, Env),
    {{data, H}, Env1};
call({fn, cdr}, [X], Env) ->
    {{data, [_|T]}, Env1} = apply(X, Env),
    {{data, T}, Env1};
call({fn, cons}, [X,Y], Env) ->
    {[{data,X1}, {data,Y1}], Env1} =
        eval_list([X,Y], Env),
    {{data, [X1|Y1]}, Env1};
call({fn, 'cond'}, [[[P,E]|T]], Env) ->
    {{data, R}, Env1} = apply(P, Env),
    case R of
        false ->
            call({fn, 'cond'}, [T], Env1);
        true ->
            apply(E, Env1)
    end;
call({fn, atom}, [X], Env) ->
    {{data, X1}, Env1} = apply(X, Env),
    {{data, is_atom(X1)}, Env1};
call({fn, eq}, [X,Y], Env) ->
    {[{data,X1}, {data,Y1}], Env1} =
        eval_list([X,Y], Env),
    {{data, eq(X1,Y1)}, Env1}.


new_env() ->
    [{quote, {fn, quote}},
     {label, {fn, label}},
     {car,   {fn, car}},
     {cdr,   {fn, cdr}},
     {cons,  {fn, cons}},
     {'cond',{fn, 'cond'}},
     {atom,  {fn, atom}},
     {eq,    {fn, eq}}].


eval_list([], Env) ->
    {[], Env};
eval_list([H|T], Env) ->
    {VH, Env1} = apply(H, Env),
    {VT, Env2} = eval_list(T, Env1),
    {[VH|VT], Env2}.


eq([], []) ->
    true;
eq(X, Y)
  when is_atom(X), is_atom(Y) ->
    X =:= Y;
eq(_, _) ->
    false.


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
                  ], new_env());
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
              new_env());
test('cond') ->
    {{data, a}, _} =
        apply(['cond',
               [[[quote, true],
                 [quote, a]],
                [[quote, false],
                 [quote, b]],
                [[quote, false],
                 [quote, c]]
               ]], new_env()),
    {{data, b}, _} =
        apply(['cond',
               [[[quote, false],
                 [quote, a]],
                [[quote, true],
                 [quote, b]],
                [[quote, false],
                 [quote, c]]
               ]], new_env()),
    {{data, c}, _} =
        apply(['cond',
               [[[quote, false],
                 [quote, a]],
                [[quote, false],
                 [quote, b]],
                [[quote, true],
                 [quote, c]]
               ]], new_env());
test(atom) ->
    {{data, true}, _} =
        apply([atom, [quote, a]], new_env()),
    {{data, false}, _} =
        apply([atom, [quote, [a,b,c]]], new_env());
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
              new_env()).


test() ->
    test(quote),
    test(label),
    test(list),
    test('cond'),
    test(atom),
    test(eq),
    ok.
