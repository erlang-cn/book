-module(eval_tuple).

-export([test/0]).


% SNIP BEGIN eval-integer
eval({integer, _, Value}, Bindings) ->
    {ok, Value, Bindings}
%- SNIP END
;
% SNIP BEGIN eval-atom
eval({atom, _, Value}, Bindings) ->
    {ok, Value, Bindings}
%- SNIP END
;
% SNIP BEGIN eval-var
eval({var, _, Var}, Bindings) ->
    case bindings:lookup(Var, Bindings) of
        {ok, Value} ->
            {ok, Value, Bindings};
        none ->
            {error, {unbound, Var}}
    end
%- SNIP END
;
% SNIP BEGIN eval-list
eval({nil, _}, Bindings) ->
    {ok, [], Bindings};
eval({cons, _, H, T}, Bindings) ->
    case eval(H, Bindings) of
        {ok, H1, Bindings1} ->
            case eval(T, Bindings1) of
                {ok, T1, Bindings2} ->
                    {ok, [H1|T1], Bindings2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end
%- SNIP END
;
% SNIP BEGIN eval-tuple
eval({tuple, _, Elements}, Bindings) ->
    case eval_elements(Elements, Bindings) of
        {ok, Value, Bindings1} ->
            {ok, {tuple, Value}, Bindings1};
        Error ->
            Error
    end
%- SNIP END
.

% SNIP BEGIN eval_elements
eval_elements([], Bindings) ->
    {ok, [], Bindings};
eval_elements([H|T], Bindings) ->
    case eval(H, Bindings) of
        {ok, H1, Bindings1} ->
            case eval_elements(T, Bindings1) of
                {ok, T1, Bindings2} ->
                    {ok, [H1|T1], Bindings2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
% SNIP END

% SNIP BEGIN eval_string
eval_string(S, Bindings) ->
    eval(parse_util:expr(S), Bindings).
% SNIP END

% SNIP BEGIN eval-test-integer
test(eval_integer) ->
    {ok, 1, []} = eval_string("1.", []),
    {ok, 2, []} = eval_string("2.", []),
    {ok, 3, []} = eval_string("3.", []),
    ok
%- SNIP END
;
% SNIP BEGIN eval-test-atom
test(eval_atom) ->
    {ok, a, []} = eval_string("a.", []),
    {ok, b, []} = eval_string("b.", []),
    {ok, c, []} = eval_string("c.", []),
    ok
%- SNIP END
;
% SNIP BEGIN eval-test-var
test(eval_var) ->
    {ok, 1, [{'X', 1}]} = eval_string("X.", [{'X', 1}]),
    {error, {unbound, 'X'}} = eval_string("X.", []),
    ok
%- SNIP END
;
% SNIP BEGIN eval-test-list
test(eval_list) ->
    {ok, [], []} = eval_string("[].", []),
    {ok, [1,2], []} = eval_string("[1,2].", []),
    {ok, [1,2], [{'X', 1}, {'Y', 2}]} = eval_string("[X,Y].", [{'X', 1}, {'Y', 2}]),
    ok
%- SNIP END
;
% SNIP BEGIN eval-test-tuple
test(eval_tuple) ->
    {ok, {tuple, [1,2]}, []} = eval_string("{1,2}.", []),
    {ok, {tuple, [1,2]}, [{'X', 1}, {'Y', 2}]} = eval_string("{X,Y}.", [{'X', 1}, {'Y', 2}]),
    ok
%- SNIP END
.

test() ->
    test(eval_integer),
    test(eval_atom),
    test(eval_var),
    test(eval_list),
    test(eval_tuple),
    ok.
