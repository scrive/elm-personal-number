# Elm Personal Number

[![Elm CI](https://github.com/scrive/elm-personal-number/workflows/Elm%20CI/badge.svg)](https://github.com/scrive/elm-personal-number/actions)

A library for validating personal numbers. Built from the ground up using parser combinators!

## Supported countries

- Swedish personal numbers (personnummer).
- Swedish personal numbers for foreign visitors (sammordningsnummer).
- Norwegian personal numbers (fødselsnummer).
- Finnish personal numbers (henkilötunnus).
- Danish personal numbers (personnummer).

## Example usage

```elm
import PersonalNumber.Swedish as PersonalNumber exposing (ValidationError(..))


personalNumber : String -> String
personalNumber str =
  case PersonalNumber.fromString str of
    Ok pnr ->
      -- The `pnr` is wrapped in a `PersonalNumber` type and is
      -- guaranteed to be valid. Use the `display` function to turn
      -- it back into a user readable string.
      PersonalNumber.display pnr

    Err InvalidFormat ->
      "Not the correct format for a swedish personal number."

    Err InvalidLength ->
      "The personal number is not of the correct length."

    Err InvalidDate ->
      "The first part of the personal number needs to be a valid date."

    Err InvalidChecksum ->
      "One or more digits in the personal number is wrong."
```

## JSON

```elm
import PersonalNumber.Swedish as PersonalNumber
import Json.Decode as Decode
import Json.Encode as Encode

type alias Person =
  { personalNumber : PersonalNumber.PersonalNumber
  }

decoder : Decoder Person
decoder =
  Decode.map2 Person
    (Decode.field "personal_number" PersonalNumber.decoder)


encode : Person -> Value
encode person =
  Encode.object
    [ ( "personal_number", PersonalNumber.encode person.personalNumber )
    ]
```
