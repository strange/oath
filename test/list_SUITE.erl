-module(list_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).

all() ->
    [
        convert
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, [a, b, c]} = oath:validate([a, b, c], list, []),

    ok.
