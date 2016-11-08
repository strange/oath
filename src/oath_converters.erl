-module(oath_converters).

-export([map_converter/2]).
-export([string_converter/2]).
-export([list_converter/2]).
-export([integer_converter/2]).
-export([float_converter/2]).

map_converter(Value, _Props) when is_map(Value) ->
    {ok, Value};
map_converter(Value, #{ strict := false }) when is_list(Value) ->
    case catch maps:from_list(Value) of
        {'EXIT', _} ->
            {error, invalid_map};
        MapValue ->
            {ok, MapValue}
    end;
map_converter(_Value, _Props) ->
    {error, invalid_map}.

list_converter(Value, _Props) when is_list(Value) ->
    {ok, Value};
list_converter(_Value, _Props) ->
    {error, invalid_list}.

%% @doc Attempt to convert value to a string
string_converter(S, #{ strict := true }) when not is_list(S) ->
    {error, invalid_string};
string_converter(S, _Props) when is_binary(S) ->
    case unicode:characters_to_list(S, utf8) of
        {error, _Encoded, _Rest} ->
            {error, invalid_string};
        List when is_list(List) ->
            {ok, List}
    end;
string_converter(S, _Props) when is_list(S) ->
    case unicode:characters_to_binary(S, utf8) of
        {error, _Encoded, _Rest} ->
            {error, invalid_string};
        Bin when is_binary(Bin) ->
            {ok, S}
    end;
string_converter(_S, _Props) ->
    {error, invalid_string}.

%% @doc Attempt to convert value to an integer
integer_converter(S, #{ strict := true }) when not is_integer(S) ->
    {error, invalid_integer};
integer_converter(S, _Props) when is_integer(S) ->
    {ok, S};
integer_converter(S, Props) when is_list(S) ->
    integer_converter(list_to_binary(S), Props);
integer_converter(S, _Props) when is_binary(S) ->
    case catch binary_to_integer(S) of
        {'EXIT', {badarg, _}} ->
            {error, invalid_integer};
        Converted ->
            {ok, Converted}
    end;
integer_converter(_S, _Props) ->
    {error, invalid_integer}.

%% @doc Attempt to convert value to a float
float_converter(S, #{ strict := true }) when not is_float(S) ->
    {error, invalid_float};
float_converter(S, _Props) when is_float(S) ->
    {ok, S};
float_converter(S, Props) when is_list(S) ->
    float_converter(list_to_binary(S), Props);
float_converter(S, _Props) when is_binary(S) ->
    case catch binary_to_float(S) of
        {'EXIT', {badarg, _}} ->
            {error, invalid_float};
        Converted ->
            {ok, Converted}
    end;
float_converter(_S, _Props) ->
    {error, invalid_float}.
