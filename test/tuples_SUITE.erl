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
    %% Rules = [
    %%     {<<"name">>, string, [{required, true}]},
    %%     {<<"gender">>, string, #{
    %%         required => true,
    %%         max_length => 1,
    %%         in => ["m", "f"]
    %%     }}
    %% ],

    %% {ok, #{<<"gender">> := "m", <<"name">> := "Gurra"}} = oath:validate_tuples([
    %%     {<<"name">>, <<"Gurra">>},
    %%     {<<"gender">>, <<"m">>}
    %% ], Rules),
    %%
    %% {error, invalid_tuples} = oath:validate_tuples([
    %%     <<"invalid">>
    %% ], Rules),
    %%
    %%
    %% {error, #{<<"gender">> := not_in_values}} = oath:validate_tuples([
    %%     {<<"name">>, <<"Gurra">>},
    %%     {<<"gender">>, <<"x">>}
    %% ], Rules),

    ok.
