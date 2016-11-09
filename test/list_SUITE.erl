-module(list_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([length/1]).

all() ->
    [
        convert,
        length
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, [a, b, c]} = oath:validate([a, b, c], list, []),
    ok.

length(_Config) ->
    {ok, [a, b, c]} = oath:validate([a, b, c], list, [{min_length, 3}]),
    {error, invalid_length} = oath:validate([a, b, c], list, [{min_length, 4}]),

    {ok, [a, b, c]} = oath:validate([a, b, c], list, [{max_length, 3}]),
    {error, invalid_length} = oath:validate([a, b, c], list, [{max_length, 2}]),
    ok.
