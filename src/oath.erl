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
        ]},
        {list, [
            fun oath_validators:empty_check/2,
            fun oath_converters:list_converter/2,
            fun oath_validators:min_length_validator/2,
            fun oath_validators:max_length_validator/2
        ]},
        {tuples, [
            fun oath_validators:ruleset_tuples_validator/2
        ]},
        {map, [
            fun oath_validators:ruleset_tuples_validator/2
        ]},
        {binary, [
            fun oath_validators:empty_check/2,
            fun oath_validators:min_size_validator/2,
            fun oath_validators:max_size_validator/2,
            fun oath_validators:value_in_validator/2
        ]},
        {integer, [
            fun oath_validators:strip/2,
            fun oath_validators:empty_check/2,
            fun oath_converters:integer_converter/2,
            fun oath_validators:greater_than/2,
            fun oath_validators:greater_than_or_equal_to/2,
            fun oath_validators:less_than/2,
            fun oath_validators:less_than_or_equal_to/2,
            fun oath_validators:equal_to/2,
            fun oath_validators:value_in_validator/2
        ]},
        {float, [
            fun oath_validators:strip/2,
            fun oath_validators:empty_check/2,
            fun oath_converters:float_converter/2,
            fun oath_validators:greater_than/2,
            fun oath_validators:greater_than_or_equal_to/2,
            fun oath_validators:less_than/2,
            fun oath_validators:less_than_or_equal_to/2,
            fun oath_validators:equal_to/2,
            fun oath_validators:value_in_validator/2
        ]},
        {url, [
            string,
            fun oath_validators:valid_url/2
        ]}
    ].

validate_tuples(Data, Rules) ->
    validate(Data, tuples, [{rules, Rules}]).

validate_map(Data, Rules) ->
    validate(Data, map, [{rules, Rules}]).

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
            run_validation(NewValue, T, Props);
        {invalid, Errors} ->
            {error, Errors};
        {valid, Values} ->
            {ok, Values};
        Other ->
            io:format("Other: ~p~n", [Other])
    end.

get_validators(Type) ->
    {Type, Validators} = lists:keyfind(Type, 1, validators()),
    Validators.
