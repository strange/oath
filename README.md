# Oath - Erlang term validation

## Usage

Basic usage of `oath:validate/3`:

    {error, invalid_length} = oath:validate(<<"aa">>, string, [{max_length, 1}]).

Validate deep terms:

    Rules = [
        {<<"name">>, string, #{min_length = 1}},
        {<<"gender">>, string, #{
            required => true,
            max_length => 1,
            in => ["m", "f"]
        }}
    ].

    {error, Errors} = oath:validate([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"x">>}
    ], proplist, Rules).

## Ideas

- Figure out how to coerce types
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
- IP (4 and 6)

## Coercion

<<"string">> -> "string" | <<"string">>
"string" -> "string" | <<"string">>
undefined -> undefined | "" | <<>>

binary -> binary
undefined -> undefined | <<>>

int -> int
"int" -> int
<<"int">> -> int
-> undefined | <<>> | ""

float -> float
"float" -> float
<<"float">> -> float
-> undefined | <<>> | ""

"date" -> {Date}
{Date} -> {Date}
-> undefined | <<>> | ""

[] -> []

[{}] -> [{}]

#{} -> #{}

---

