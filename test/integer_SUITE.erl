-module(integer_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([empty/1]).
-export([comparison/1]).

all() ->
    [
        convert,
        empty,
        comparison
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
    {ok, undefined} = oath:validate([], integer, [{required, false}]),
    {ok, 666} = oath:validate([], integer, [{required, false}, {default, 666}]),

    ok.

comparison(_Config) ->
    {ok, 1} = oath:validate(1, integer, [{gt, 0}]),
    {ok, 1} = oath:validate(1, integer, [{gte, 1}]),

    {error, {not_greater_than, 1}} = oath:validate(1, integer, [{gt, 1}]),
    {error, {not_greater_than_or_equal_to, 2}} = oath:validate(1, integer,
                                                               [{gte, 2}]),

    {ok, 1} = oath:validate(1, integer, [{lt, 2}]),
    {ok, 1} = oath:validate(1, integer, [{lte, 1}]),

    {error, {not_less_than, 0}} = oath:validate(1, integer, [{lt, 0}]),
    {error, {not_less_than_or_equal_to, 1}} = oath:validate(2, integer,
                                                            [{lte, 1}]),

    {ok, 1} = oath:validate(1, integer, [{eq, 1}]),

    {error, {not_equal_to, 2}} = oath:validate(1, integer, [{eq, 2}]),
    {error, {not_equal_to, 1.0}} = oath:validate(1, integer, [{eq, 1.0}]),

    ok.
