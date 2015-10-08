-module(oath).

-export([validate/3]).
-export([validate_tuples/2]).
-export([validate_map/2]).

validators() -> [
        {string, [
                fun oath_validators:strip/2,
                fun oath_validators:empty_check/2,
                fun oath_converters:string_converter/2,
                fun oath_validators:min_length_validator/2,
                fun oath_validators:max_length_validator/2,
                fun oath_validators:value_in_validator/2,
                fun oath_validators:custom_validators/2
            ]
        },
        {binary, [
                fun oath_validators:empty_check/2,
                fun oath_validators:min_size_validator/2,
                fun oath_validators:max_size_validator/2,
                fun oath_validators:value_in_validator/2
            ]
        },
        {integer, [
                fun oath_validators:strip/2,
                fun oath_validators:empty_check/2,
                fun oath_converters:integer_converter/2,
                fun oath_validators:value_in_validator/2
            ]
        },
        {url, [
                string,
                fun oath_validators:valid_url/2
            ]
        }
    ].

validate_tuples(Data, Rules) ->
    validate_map(maps:from_list(Data), Rules).

validate_map(Data, Rules) ->
    validate_map(Data, Rules, #{}, #{}).

validate_map(_Data, [], Values, Errors) when map_size(Errors) =:= 0 ->
    {valid, Values};
validate_map(_Data, [], _Values, Errors) ->
    {invalid, Errors};
validate_map(Data, [{Key, Type, Props}|T], Values, Errors) ->
    case validate(maps:get(Key, Data, undefined), Type, Props) of
        {ok, Value} ->
            validate_map(Data, T, Values#{Key => Value}, Errors);
        {error, Reason} ->
            validate_map(Data, T, Values, Errors#{Key => Reason})
    end.

validate(Value, Type, Props) when is_list(Props) ->
    validate(Value, Type, maps:from_list(proplists:unfold(Props)));

validate(Value, Type, Props) ->
    Validators = get_validators(Type),
    run_validation(Value, Validators, Props).

run_validation(Value, [], _Props) ->
    {ok, Value};

run_validation(Value, [H|T], Props) when is_atom(H) ->
    Validators = get_validators(H),
    run_validation(Value, Validators ++ T, Props);

run_validation(Value, [H|T], Props) ->
    case H(Value, Props) of
        {error, _Reason} = Response ->
            Response;
        {return, NewValue} ->
            {ok, NewValue};
        {ok, NewValue} ->
            run_validation(NewValue, T, Props)
    end.

get_validators(Type) ->
    {Type, Validators} = lists:keyfind(Type, 1, validators()),
    Validators.
