-module(integer_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([empty/1]).

all() ->
    [
        convert,
        empty
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, 1} = oath:validate(<<"1">>, integer, []),
    {ok, 11} = oath:validate("11", integer, []),
    {ok, 11} = oath:validate(11, integer, []),
    {error, invalid_integer} = oath:validate("11z", integer, []),

    ok.

empty(_Config) ->
    {error, required} = oath:validate(<<>>, integer, []),
    {error, required} = oath:validate(<<>>, integer, [required]),
    {error, required} = oath:validate([], integer, []),
    {ok, []} = oath:validate([], integer, [{required, false}]),
    {ok, 666} = oath:validate([], integer, [{required, false}, {default, 666}]),

    ok.
