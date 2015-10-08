-module(string_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([empty/1]).
-export([min_length/1]).
-export([max_length/1]).
-export([custom/1]).
-export([in/1]).

all() ->
    [
        convert,
        empty,
        min_length,
        max_length,
        custom,
        in
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, "abc"} = oath:validate(<<"abc">>, string, []),
    {ok, "abc"} = oath:validate("abc", string, []),
    {ok, []} = oath:validate([], string, [{required, false}]),
    {ok, undefined} = oath:validate(undefined, string, [{required, false}]),
    {error, invalid_string} = oath:validate(abc, string, []),
    {error, invalid_string} = oath:validate(123, string, []),

    ok.

empty(_Config) ->
    {error, required} = oath:validate(<<>>, string, []),
    {error, required} = oath:validate(<<>>, string, [required]),
    {ok, <<>>} = oath:validate(<<>>, string, [{required, false}]),
    {ok, x} = oath:validate([], string, [{required, false}, {default, x}]),

    ok.

min_length(_Config) ->
    {ok, "a"} = oath:validate(<<"a">>, string, [{min_length, 1}]),
    {error, invalid_length} = oath:validate(<<"a">>, string, [{min_length, 2}]),

    ok.

max_length(_Config) ->
    {ok, "a"} = oath:validate(<<"a">>, string, [{max_length, 1}]),
    {error, invalid_length} = oath:validate(<<"aa">>, string, [{max_length, 1}]),

    ok.

in(_Config) ->
    {ok, "a"} = oath:validate(<<"a">>, string, [{in, ["a", "b"]}]),
    {ok, "b"} = oath:validate(<<"b">>, string, [{in, ["a", "b"]}]),
    {error, not_in_values} = oath:validate(<<"c">>, string, [{in, ["a", "b"]}]),

    ok.

custom(_Config) ->
    {ok, "abc"} = oath:validate(<<"abc">>, string, [{custom,
                                                     [fun(Value) ->
                                                          {ok, Value}
                                                      end]}]),
    {error, bad_user} = oath:validate(<<"abc">>, string, [{custom,
                                                           [fun(_Value) ->
                                                                {error, bad_user}
                                                            end]}]),

    ok.

