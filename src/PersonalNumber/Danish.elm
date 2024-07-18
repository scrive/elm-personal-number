module PersonalNumber.Danish exposing
    ( PersonalNumber, isFemale, isMale
    , ValidationError(..)
    , fromString, toString, display
    , decoder, encode
    )

{-| Parse Danish personal numbers, are used to identify citizens.
Hetu PIN consists of ten characters: DDMMYYXXXX / DDMMYY-XXXX,
where DDMMYY is the day, month and year of birth.


# Definition

@docs PersonalNumber, isFemale, isMale


# Errors

@docs ValidationError


# Strings

@docs fromString, toString, display


# JSON

@docs decoder, encode

-}

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), Parser, chompIf, getChompedString)
import Util exposing (digit, getSerialPart, stringLength, validateDate, whitespace)


{-| An opaque type representing a valid personal number.
-}
type PersonalNumber
    = PersonalNumber String


{-| If the parsing fails an error of this type is returned.
-}
type ValidationError
    = InvalidLength
    | InvalidDate
    | InvalidFormat


personalNumberParser : Parser PersonalNumber
personalNumberParser =
    (getChompedString <|
        Parser.succeed ()
            |. whitespace
            |. stringLength 6
            |. Parser.oneOf
                [ chompIf (\c -> c == '-')
                    |. digit
                    |. digit
                    |. digit
                    |. digit
                , digit
                    |. digit
                    |. digit
                    |. digit
                ]
    )
        |> Parser.map PersonalNumber


validateFormat : String -> Result ValidationError String
validateFormat str =
    case Parser.run personalNumberParser str of
        Ok pnr ->
            Ok (toString pnr)

        Err _ ->
            Err InvalidFormat


{-| Parse a string into a personal number.
-}
fromString : String -> Result ValidationError PersonalNumber
fromString str =
    let
        pnr : String
        pnr =
            String.trim str
    in
    case String.length (String.replace "-" "" pnr) of
        10 ->
            validateFormat pnr
                |> Result.andThen (validateDate InvalidDate)
                |> Result.map PersonalNumber

        _ ->
            Err InvalidLength


{-| Converts a personal number to string representation in the long format that is
commonly used for database storage (DDMMYYXXXX/DDMMYY-XXXX).
-}
toString : PersonalNumber -> String
toString (PersonalNumber str) =
    str


{-| Encode a personal number into a JSON value.
-}
encode : PersonalNumber -> Encode.Value
encode =
    Encode.string << toString


{-| Decode a personal number.
-}
decoder : Decode.Decoder PersonalNumber
decoder =
    let
        decode str =
            case fromString str of
                Ok pnr ->
                    Decode.succeed pnr

                Err _ ->
                    Decode.fail "Invalid personal number."
    in
    Decode.string |> Decode.andThen decode


{-| Format a personal number into a user readable string (DDMMYYXXXX/DDMMYY-XXXX).
-}
display : PersonalNumber -> String
display =
    toString


{-| Males have an odd serial number
-}
isMale : PersonalNumber -> Bool
isMale (PersonalNumber str) =
    let
        serialPart : Int
        serialPart =
            str
                |> getSerialPart
                |> String.toInt
                |> Maybe.withDefault 0
    in
    modBy 2 serialPart == 1


{-| Females have an even serial number
-}
isFemale : PersonalNumber -> Bool
isFemale =
    not << isMale
