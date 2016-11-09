-define(VALIDATORS, [
        {string, [
            fun oath_validators:strip/2,
            fun oath_validators:empty_check/2,
            fun oath_converters:string_converter/2,
            fun oath_validators:min_length_validator/2,
            fun oath_validators:max_length_validator/2,
            fun oath_validators:value_in_validator/2,
            fun oath_validators:equal_to/2,
            fun oath_validators:not_equal_to/2,
            fun oath_validators:regex/2,
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
            fun oath_validators:empty_check/2,
            fun oath_converters:map_converter/2,
            fun oath_validators:ruleset_map_validator/2
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
            fun oath_validators:not_equal_to/2,
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
            fun oath_validators:not_equal_to/2,
            fun oath_validators:value_in_validator/2
        ]},
        {url, [
            string,
            fun oath_validators:valid_url/2
        ]},
        {email, [
            string,
            fun oath_validators:valid_email/2
        ]}
        %% date
        %% datetime
        %% time
        %% decimal
        %% atom
        %% ip
    ]).

