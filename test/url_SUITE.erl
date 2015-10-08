-module(url_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([convert/1]).
-export([empty/1]).
-export([min_length/1]).
-export([max_length/1]).
-export([in/1]).
-export([valid_url/1]).

all() ->
    [
        convert,
        empty,
        min_length,
        max_length,
        in,
        valid_url
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

convert(_Config) ->
    {ok, "http://hi.se/"} = oath:validate(<<"http://hi.se/">>, url, []),
    {ok, "http://hi.se/"} = oath:validate("http://hi.se/", url, []),
    {error, invalid_string} = oath:validate('hi.se', url, []),
    ok.

empty(_Config) ->
    {error, required} = oath:validate(<<>>, url, []),
    {error, required} = oath:validate(<<>>, url, [required]),
    {ok, <<>>} = oath:validate(<<>>, url, [{required, false}]),
    {ok, x} = oath:validate([], url, [{required, false}, {default, x}]),
    ok.

min_length(_Config) ->
    {ok, "http://hi.se/"} = oath:validate("http://hi.se/", url,
                                          [{min_length, 5}]),
    {error, invalid_length} = oath:validate("http://hi.se/", url,
                                            [{min_length, 20}]),
    ok.

max_length(_Config) ->
    {ok, "http://hi.se/"} = oath:validate("http://hi.se/", url,
                                          [{max_length, 20}]),
    {error, invalid_length} = oath:validate("http://hi.se/", url,
                                            [{max_length, 5}]),
    ok.

in(_Config) ->
    {ok, "http://hi.se/"} = oath:validate(<<"http://hi.se/">>, url,
                                          [{in, ["http://hi.se/",
                                                 "http://www.hi.se/"]}]),
    ok.

valid_url(_Config) ->
    {ok, "http://hi.se/"} = oath:validate(<<"http://hi.se/">>, url, []),
    {ok, "http://hi.se/?q=a"} = oath:validate(<<"http://hi.se/?q=a">>, url, []),
    {ok, "http://test@hi.se/"} = oath:validate(<<"http://test@hi.se/">>, url, []),
    {error, invalid_url} = oath:validate(<<"hi.se/">>, url, []),
    ok.
