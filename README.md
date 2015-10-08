# This is just a test

## Value

    {error, invalid_length} = oath:validate(<<"aa">>, string, [{max_length, 1}]).

## Values as list of tuples

    Rules = [
        {<<"name">>, string, #{min_length = 1}},
        {<<"gender">>, string, #{
            required => true,
            max_length => 1,
            in => ["m", "f"]
        }}
    ].

    {invalid, Errors} = oath:validate_tuples([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"x">>}
    ], Rules).

## Ideas

- Add/replace/extend validators
- Add context (protocol, credentials, host, port, path etc to url validator
  for example.
- Option to append or remove last slash of url
- Option to add default protocol to url
- New URL validator
- Validate internal ip addresses

## Validators

- Decimal
- Date
- Time
- DateTime
- Regex
- Email
- Multiple values / list
- IP (4 and 6)
