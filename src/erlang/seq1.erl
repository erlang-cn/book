% SNIP BEGIN seq-module
-module(seq1).

-export([a/1]).
% SNIP END

% SNIP BEGIN seq-a
a(1) -> 1;
a(2) -> 2;
a(3) -> 3;
a(4) -> 4.
% SNIP END
