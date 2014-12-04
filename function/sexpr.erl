-module(sexpr).

-export([is_sexpr/1]).


is_sexpr([]) ->
    true;
is_sexpr([Head|Tail]) ->
    case is_sexpr(Head) of
        false ->
            false;
        true ->
            is_sexpr(Tail)
    end;
is_sexpr(Expr) ->
    is_atom(Expr).
