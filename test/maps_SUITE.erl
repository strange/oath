-module(maps_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([empty/1]).
-export([valid/1]).
-export([invalid/1]).

all() ->
    [
        convert,
        valid,
        invalid,
        empty
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, undefined} = oath:validate([], map, #{ required => false }),
    {ok, undefined} = oath:validate(<<>>, map, #{ required => false }),
    {ok, #{ a := b }} = oath:validate([{a, b}], map, #{ strict => false }),
    ok.

empty(_Config) ->
    {error, required} = oath:validate(<<>>, map, #{}),
    {ok, #{}} = oath:validate(<<>>, map, #{ default => #{} }),
    ok.

invalid(_Config) ->
    {ok, #{ a := #{ b := 1 }}} = oath:validate(#{ a => #{ b => 1 } }, map, #{
        rules => [{a, map, #{ rules => [{b, integer, #{}}] }}]
    }),
    ok.

valid(_Config) ->
    {ok, #{ a := b }} = oath:validate(#{ a => b }, map, []),
    ok.
