# Oath - Erlang term validation

## Usage

Basic usage of `oath:validate/3`:

    {error, invalid_length} = oath:validate(<<"aa">>, string, #{
        max_length => 1
    }).

Validate deep terms:

    Rules = [
        {<<"name">>, string, #{ min_length = 1 }},
        {<<"gender">>, string, #{
            required => true,
            max_length => 1,
            in => ["m", "f", "x"]
        }}
    ].

    {error, Errors} = oath:validate([
        {<<"name">>, <<"Gurra">>},
        {<<"gender">>, <<"x">>}
    ], proplist, #{ rules => Rules }).

## Concept

The basic concept is that a _value_ is sent in alongside a _type_. The _type_
determines which _validators_ are available. A _validator_ is a function with
an arity of two that is invoked with the value and a _configuration_.

- Value: the value to be validated

## Types

The most commonly used fun is `validate/3`. It takes three arguments: a value,
a type and an optional configuration.

The value is the raw data to be validated. The type is an atom defining the
type of value to be validated (`string`, for example), and configuration is
specified as a map containing settings for different validators (a max length
for example).

What to configure depends on the validators available for a specific type.

### String

Type: `string`.

Configuration:

    #{
        max_length => N,
        min_length => N,

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

