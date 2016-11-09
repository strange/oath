-module(email_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([empty/1]).
-export([min_length/1]).
-export([max_length/1]).
-export([in/1]).
-export([valid_email/1]).

all() ->
    [
        convert,
        empty,
        min_length,
        max_length,
        in,
        valid_email
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, "gs@trell.se"} = oath:validate(<<"gs@trell.se">>, email),
    {ok, "gs@trell.se"} = oath:validate("gs@trell.se", email),
    {error, invalid_string} = oath:validate('gs@trell.se', email),
    ok.

empty(_Config) ->
    {error, required} = oath:validate(<<>>, email),
    {error, required} = oath:validate(<<>>, email, [required]),
    {ok, undefined} = oath:validate(<<>>, email, [{required, false}]),
    {ok, x} = oath:validate([], email, [{required, false}, {default, x}]),
    ok.

min_length(_Config) ->
    {ok, "gs@trell.se"} = oath:validate("gs@trell.se", email,
                                          [{min_length, 5}]),
    {error, invalid_length} = oath:validate("gs@trell.se", email,
                                            [{min_length, 20}]),
    ok.

max_length(_Config) ->
    {ok, "gs@trell.se"} = oath:validate("gs@trell.se", email,
                                          [{max_length, 20}]),
    {error, invalid_length} = oath:validate("gs@trell.se", email,
                                            [{max_length, 5}]),
    ok.

in(_Config) ->
    {ok, "gs@trell.se"} = oath:validate(<<"gs@trell.se">>, email,
                                          [{in, ["gs@trell.se",
                                                 "mw.se"]}]),
    ok.

valid_email(_Config) ->
    {ok, "gs@trell.se"} = oath:validate(<<"gs@trell.se">>, email, []),
    {ok, "gs@trell.se"} = oath:validate(<<"gs@trell.se">>, email, []),
    {ok, "gs@trell.se"} = oath:validate(<<"gs@trell.se">>, email, []),
    {error, invalid_email} = oath:validate(<<"@trell.se">>, email, []),
    ok.
