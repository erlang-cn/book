-module(seq2).

-export([a/1]).

% SNIP BEGIN seq-conflict
a(1) -> 1;
a(2) -> 2;
a(3) -> 3;
a(4) -> 4;
a(4) -> 5.
% SNIP END
