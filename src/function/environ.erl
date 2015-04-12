-module(environ).

-export([empty/0, bind/3, is_bound/2, get_value/2]).

% SNIP BEGIN ans-environ
empty() ->
    [].

bind(Name, Value, Env) ->
    [{Name,Value}|Env].

is_bound(_, []) ->
    false;
is_bound(Name, [{Name, _}|_]) ->
    true;
is_bound(Name, [_|Env]) ->
    is_bound(Name, Env).

get_value(Name, [{Name, Value}|_]) ->
    Value;
get_value(Name, [_|Env]) ->
    get_value(Name, Env).
% SNIP END
