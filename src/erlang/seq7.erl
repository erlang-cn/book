-module(seq7).

-export([a/1, test/0]).

% SNIP BEGIN seq-a-case
a(N) ->
    case N of
        1 -> 1;
        2 -> 2;
        3 -> 3;
        4 -> 4
    end.
% SNIP END

% SNIP BEGIN seq-test
test() ->
    1 = a(1),
    2 = a(2),
    3 = a(3),
    4 = a(4),
    ok.
% SNIP END
