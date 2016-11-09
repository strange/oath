-module(oath).

-export([validate/2]).
-export([validate/3]).
-export([validate/4]).

-include("oath.hrl").

%% External API

validate(Value, Type) ->
    validate(Value, Type, #{}).

-spec validate(any(), atom(), list() | map()) -> {valid, any()} | {invalid, any()}.
validate(Value, Type, Props) when is_list(Props) ->
    validate(Value, Type, maps:from_list(proplists:unfold(Props)));

validate(Value, Type, Properties) ->
    validate(Value, Type, Properties, #{}).

validate(Value, Type, Properties, _DefaultProperties) ->
    Validators = get_validators(Type),
    perform_validation(Value, Validators, Properties).

%% Internal API

perform_validation(Value, [], _Props) ->
    {ok, Value};

perform_validation(Value, [SuperType|Rest], Props) when is_atom(SuperType) ->
    Validators = get_validators(SuperType),
    perform_validation(Value, Validators ++ Rest, Props);

perform_validation(Value, [Validator|Rest], Props) ->
    case Validator(Value, Props) of
        {error, _Reason} = Response ->
            Response;
        {return, NewValue} ->
            {ok, NewValue};
        {ok, NewValue} ->
            perform_validation(NewValue, Rest, Props);
        {invalid, Errors} ->
            {error, Errors};
        {valid, Values} ->
            {ok, Values};
        Other ->
            io:format("Other: ~p~n", [Other])
    end.

get_validators(Type) ->
    {Type, Validators} = lists:keyfind(Type, 1, ?VALIDATORS),
    Validators.
