-module(tuples_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([tuples/1]).

all() ->
    [
        tuples
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

tuples(_Config) ->
    ValidationSet = [
        {<<"name">>, string, [{required, true}]},
        {<<"gender">>, string, [{required, true}, {max_length, 1},
                                {in, ["m", "f"]}]}
    ],

    {valid, #{<<"gender">> := "m"}} = oath:validate_tuples([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"m">>}
    ], ValidationSet),

    {invalid, #{<<"gender">> := not_in_values}} = oath:validate_tuples([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"x">>}
    ], ValidationSet),

    ok.
