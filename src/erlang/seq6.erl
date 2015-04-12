-module(seq6).

-export([c/1]).

% SNIP BEGIN seq-c-guard
c(N) when N >= 1 -> N.
% SNIP END
