-module(seq5).

-export([b/1]).

% SNIP BEGIN seq-b-guard
b(N) when N >= 1 -> 1.
% SNIP END
