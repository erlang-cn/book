-module(board1).

-export([test/0, move/2]).

move(left, {X,Y})
  when X > 1, X =< 8 ->
    {X-1, Y};
move(right, {X,Y})
  when X >= 1, X < 8 ->
    {X+1, Y};
move(up, {X,Y})
  when Y > 1, Y =< 8 ->
    {X, Y-1};
move(down, {X,Y})
  when Y >= 1, Y < 8 ->
    {X, Y+1}.


test(move) ->
    A = {3,3},
    B = {4,3},
    C = {3,4},
    B = move(right, A),
    A = move(left, B),
    C = move(down, A),
    A = move(up, C).


test() ->
    test(move),
    ok.
