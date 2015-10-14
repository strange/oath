-module(float_SUITE).

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
    {ok, 1.0} = oath:validate(<<"1.0">>, float, []),
    {ok, 11.0} = oath:validate("11.0", float, []),
    {error, invalid_float} = oath:validate("11z", float, []),

    ok.

empty(_Config) ->
    {error, required} = oath:validate(<<>>, float, []),
    {error, required} = oath:validate(<<>>, float, [required]),
    {error, required} = oath:validate([], float, []),
    {ok, []} = oath:validate([], float, [{required, false}]),
    {ok, 666} = oath:validate([], float, [{required, false}, {default, 666}]),

    ok.

comparison(_Config) ->
    {ok, 1.0} = oath:validate(1.0, float, [{gt, 0}]),
    {ok, 1.0} = oath:validate(1.0, float, [{gte, 1}]),

    {error, {not_greater_than, 1}} = oath:validate(1, integer, [{gt, 1}]),
    {error, {not_greater_than_or_equal_to, 2}} = oath:validate(1, integer,
                                                               [{gte, 2}]),

    {ok, 1.0} = oath:validate(1.0, float, [{lt, 2}]),
    {ok, 1.0} = oath:validate(1.0, float, [{lte, 1}]),

    {error, {not_less_than, 0}} = oath:validate(1.0, float, [{lt, 0}]),
    {error, {not_less_than_or_equal_to, 1}} = oath:validate(2.0, float,
                                                            [{lte, 1}]),

    {ok, 1.0} = oath:validate(1.0, float, [{eq, 1.0}]),

    {error, {not_equal_to, 2}} = oath:validate(1.0, float, [{eq, 2}]),
    {error, {not_equal_to, 1}} = oath:validate(1.0, float, [{eq, 1}]),

    ok.
