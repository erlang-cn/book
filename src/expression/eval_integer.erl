-module(eval_integer).

-export([test/0]).


% SNIP BEGIN eval-integer
eval({integer, _, Value}, Bindings) ->
    {ok, Value, Bindings}
%- SNIP END
.

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
.

test() ->
    test(eval_integer),
    ok.
