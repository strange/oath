-module(oath_validators).

-export([empty_check/2]).
-export([max_length_validator/2]).
-export([max_size_validator/2]).
-export([min_length_validator/2]).
-export([min_size_validator/2]).
-export([valid_url/2]).
-export([valid_email/2]).
-export([value_in_validator/2]).
-export([custom_validators/2]).
-export([strip/2]).
-export([ruleset_tuples_validator/2]).
-export([ruleset_map_validator/2]).
-export([greater_than/2]).
-export([greater_than_or_equal_to/2]).
-export([less_than/2]).
-export([less_than_or_equal_to/2]).
-export([equal_to/2]).
-export([not_equal_to/2]).

-define(EMPTY_VALUES, [[], <<>>, undefined, null]).
-define(DEFAULT_EMPTY_VALUE, undefined).

%% http://stackoverflow.com/questions/11718898/check-string-for-email-with-regular-expressions-or-other-way
-define(EMAIL_RE, "\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*\\b").

%% @doc Strip leading and trailing blanks from value
strip(Value, #{strip := false}) ->
    {ok, Value};
strip(Value, _Props) when is_binary(Value) ->
    {ok, list_to_binary(string:strip(binary_to_list(Value)))};
strip(Value, _Props) when is_list(Value) ->
    {ok, string:strip(Value)};
strip(Value, _Props) ->
    {ok, Value}.

%% @doc Validate that value is greater than V
greater_than(Value, #{gt := V}) when V >= Value ->
    {error, {not_greater_than, V}};
greater_than(Value, _Properties) ->
    {ok, Value}.

%% @doc Validate that value is greater than or equal to V
greater_than_or_equal_to(Value, #{gte := V}) when V > Value ->
    {error, {not_greater_than_or_equal_to, V}};
greater_than_or_equal_to(Value, _Properties) ->
    {ok, Value}.

%% @doc Validate that value is less than V
less_than(Value, #{lt := V}) when V =< Value ->
    {error, {not_less_than, V}};
less_than(Value, _Properties) ->
    {ok, Value}.

%% @doc Validate that value is greater than or equal to V
less_than_or_equal_to(Value, #{lte := V}) when V < Value ->
    {error, {not_less_than_or_equal_to, V}};
less_than_or_equal_to(Value, _Properties) ->
    {ok, Value}.

%% @doc Validate that value is equal to
equal_to(Value, #{eq := V}) when Value =/= V ->
    {error, {not_equal_to, V}};
equal_to(Value, _Properties) ->
    {ok, Value}.

%% @doc Validate that value is not equal to
not_equal_to(Value, #{not_eq := V}) when Value =:= V ->
    {error, {equal_to, V}};
not_equal_to(Value, _Properties) ->
    {ok, Value}.

%% @doc Apply all custom validators on value
custom_validators(Value, #{custom := []}) ->
    {ok, Value};
custom_validators(Value, #{custom := [H|T]} = Props) ->
    case H(Value) of
        {ok, Value} ->
            custom_validators(Value, Props#{custom := T});
        {error, Reason} ->
            {error, Reason};
        {return, Value} ->
            {return, Value}
    end;
custom_validators(Value, _Props) ->
    {ok, Value}.

%% @doc Validate a list of tuples against specified rules
ruleset_tuples_validator(Value, #{rules := _Rules} = Properties) ->
    ruleset_map_validator(maps:from_list(Value), Properties);
ruleset_tuples_validator(Value, _Properties) ->
    {ok, Value}.

%% @doc Validate map against specified rules
ruleset_map_validator(Value, #{rules := Rules}) ->
    ruleset_map_validator(Value, Rules, #{}, #{});
ruleset_map_validator(Value, _Properties) ->
    {ok, Value}.
ruleset_map_validator(_Data, [], Values, Errors) when map_size(Errors) =:= 0 ->
    {valid, Values};
ruleset_map_validator(_Data, [], _Values, Errors) ->
    {invalid, Errors};
ruleset_map_validator(Data, [{Key, Type, Props}|T], Values, Errors) ->
    case oath:validate(maps:get(Key, Data, undefined), Type, Props) of
        {ok, Value} ->
            ruleset_map_validator(Data, T, Values#{Key => Value}, Errors);
        {error, Reason} ->
            ruleset_map_validator(Data, T, Values, Errors#{Key => Reason})
    end.

%% @doc Validate that the value is a valid URL
valid_url(Value, _) ->
    case http_uri:parse(Value) of
        {ok, _Parts} ->
            {ok, Value};
        _Other ->
            {error, invalid_url}
    end.

%% @doc Validate that the value is a valid email address
valid_email(Value, _) ->
    % ?EMAIL_RE was just copy/pasted from SO. i haven't thoroughly evaluated
    % how it performs. the test should probably be replaced with a custom
    % email validation module.
    case re:run(Value, ?EMAIL_RE) of
        {match, _} -> {ok, Value};
        _ -> {error, invalid_email}
    end.

%% @doc Validate that list-type is at-least N elements long
min_length_validator(Value, #{min_length := N}) when length(Value) < N ->
    {error, invalid_length};
min_length_validator(Value, _Props) ->
    {ok, Value}.

%% @doc Validate that binary is at-least N bytes
min_size_validator(Value, #{min_size := N}) when size(Value) < N ->
    {error, invalid_size};
min_size_validator(Value, _Props) ->
    {ok, Value}.

%% @doc Validate that list-type is no more than N elements long
max_length_validator(Value, #{max_length := N}) when length(Value) > N ->
    {error, invalid_length};
max_length_validator(Value, _Props) ->
    {ok, Value}.

%% @doc Validate that binary is no bigger than N bytes
max_size_validator(Value, #{max_size := N}) when size(Value) > N ->
    {error, invalid_size};
max_size_validator(Value, _Props) ->
    {ok, Value}.

%% @doc Validate that value is a member of a list of values
value_in_validator(Value, #{in := Values}) when is_list(Values) ->
    case lists:member(Value, Values) of
        true ->
            {ok, Value};
        false ->
            {error, not_in_values}
    end;
value_in_validator(_Value, #{in := Values}) when not is_list(Values) ->
    {error, invalid_configuration};
value_in_validator(Value, _Props) ->
    {ok, Value}.

%% @doc Check if value is "empty" and if an error should be returned, a
%% default value should be chosen, or if the value should be considered
%% invalid.
empty_check(Value, Properties) ->
    EmptyValues = maps:get(empty_values, Properties, ?EMPTY_VALUES),
    case {lists:member(Value, EmptyValues), Properties} of
        {false, _Props} ->
            {ok, Value};
        {true, #{required := false, default := Default}} ->
            {return, Default};
        {true, #{required := false}} ->
            {return, ?DEFAULT_EMPTY_VALUE};
        {true, _Props} ->
            {error, required}
    end.
