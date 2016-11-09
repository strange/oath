-module(list_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([multi/1]).

all() ->
    [
        convert,
        multi
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, [a, b, c]} = oath:validate([a, b, c], list, []),

    ok.

multi(_Config) ->
    {error, #{<<"age">> := not_in_values}} = oath:validate([{<<"age">>, 11}], proplist, [{rules, [
        {<<"age">>, integer, [{in, [10]}]}
    ]}]),

    {error, #{<<"age">> := not_in_values, <<"name">> := required}} =
      oath:validate([{<<"age">>, 11}], proplist, [{rules, [
        {<<"name">>, string, []},
        {<<"age">>, integer, [{in, [10]}]}
    ]}]),

    {ok, #{<<"age">> := 1}} = oath:validate([{<<"age">>, 1}], proplist, [{rules, [
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
