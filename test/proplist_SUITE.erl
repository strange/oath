-module(proplist_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([multi/1]).
-export([misc/1]).
-export([convert/1]).
-export([empty/1]).

all() ->
    [
        multi,
        misc,
        convert,
        empty
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

empty(_Config) ->
    Rules = [
        {<<"name">>, string, #{}},
        {<<"gender">>, string, #{
            max_length => 1,
            in => ["m", "f"]
        }}
    ],
    {error, #{<<"name">> := required}} = 
        oath:validate([], proplist, #{ rules => Rules }),
    ok.

misc(_Config) ->
    Rules = [
        {<<"name">>, string, #{}},
        {<<"gender">>, string, #{
            max_length => 1,
            in => ["m", "f"]
        }}
    ],

    {ok, [{<<"name">>, "Gurra"}, {<<"gender">>, "m"}]} = oath:validate([
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

convert(_Config) ->
    ok.

multi(_Config) ->
    {error, #{<<"age">> := not_in_values}} = oath:validate(
        [{<<"age">>, 11}],
        proplist,
        #{ rules => [{<<"age">>, integer, [{in, [10]}]}] }
    ),

    {error, #{<<"age">> := not_in_values, <<"name">> := required}} =
      oath:validate([{<<"age">>, 11}], proplist, #{ rules => [
        {<<"name">>, string, []},
        {<<"age">>, integer, [{in, [10]}]}
    ]}),

    {ok, [{<<"age">>, 1}]} = oath:validate([{<<"age">>, 1}], proplist, [{rules, [
        {<<"age">>, integer, []}
    ]}]),

    {error, #{<<"properties">> := #{<<"value1">> := invalid_string}}} = oath:validate([
        {<<"age">>, 1},
        {<<"properties">>, [{<<"value1">>, 1}]}
    ], proplist, [{rules, [
        {<<"age">>, integer, []},
        {<<"properties">>, proplist, [{rules, [
            {<<"value1">>, string, []}
        ]}]}
    ]}]),

    ok.
