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
    Rules = [
        {<<"name">>, string, #{}},
        {<<"gender">>, string, #{
            max_length => 1,
            in => ["m", "f"]
        }}
    ],

    {ok, #{<<"gender">> := "m", <<"name">> := "Gurra"}} = oath:validate([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"m">>}
    ], proplist, #{ rules => Rules }),


    {error, invalid_proplist} = oath:validate([
        <<"invalid">>
    ], proplist, #{ rules => Rules }),


    {error, #{<<"gender">> := not_in_values}} = oath:validate([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"x">>}
    ], proplist, #{ rules => Rules }),

    ok.
