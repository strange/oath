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
    apply_validators(Value, Validators, Properties).

%% Internal API

apply_validators(Value, [], _Props) ->
    {ok, Value};

apply_validators(Value, [SuperType|Rest], Props) when is_atom(SuperType) ->
    Validators = get_validators(SuperType),
    apply_validators(Value, Validators ++ Rest, Props);

apply_validators(Value, [Validator|Rest], Props) ->
    case Validator(Value, Props) of
        {return, NewValue} ->
            {ok, NewValue};
        {error, Errors} ->
            {error, Errors};
        {ok, NewValue} ->
            apply_validators(NewValue, Rest, Props);
        Other ->
            io:format("Other: ~p~n", [Other])
    end.

get_validators(Type) ->
    case lists:keyfind(Type, 1, ?VALIDATORS) of
        {Type, Validators} ->
            Validators;
        false ->
            erlang:error(badarg)
    end.
