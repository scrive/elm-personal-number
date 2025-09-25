module PersonalNumber.Norwegian exposing
    ( PersonalNumber
    , ValidationError(..)
    , fromString, toString, display
    , decoder, encode
    )

{-| Parse Norwegian personal numbers.


# Definition

@docs PersonalNumber


# Errors

@docs ValidationError


# Strings

@docs fromString, toString, display


# JSON

@docs decoder, encode

-}

import Json.Decode
import Json.Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), Parser, getChompedString)
import Util exposing (stringLength, validateDate, whitespace)


{-| An opaque type representing a valid personal number.
-}
type PersonalNumber
    = PersonalNumber String


{-| If the parsing fails an error of this type is returned.
-}
type ValidationError
    = InvalidFormat
    | InvalidLength
    | InvalidDate
    | InvalidChecksum


weights1 : List Int
weights1 =
    [ 3, 7, 6, 1, 8, 9, 4, 5, 2 ]


weights2 : List Int
weights2 =
    [ 5, 4, 3, 2, 7, 6, 5, 4, 3, 2 ]


personalNumberParser : Parser PersonalNumber
personalNumberParser =
    (getChompedString <|
        Parser.succeed ()
            |. whitespace
            |. stringLength 6
            |. stringLength 5
    )
        |> Parser.map PersonalNumber


weighted : List Int -> Int -> Int -> Int
weighted coefficients index value =
    coefficients
        |> List.getAt index
        |> Maybe.unwrap -1 ((*) value)


checkDigit : Int -> Int
checkDigit n =
    modBy 11 (11 - modBy 11 n)


validateFormat : String -> Result ValidationError String
validateFormat str =
    case Parser.run personalNumberParser str of
        Ok pnr ->
            Ok (toString pnr)

        Err _ ->
            Err InvalidFormat


validateChecksum : String -> Result ValidationError String
validateChecksum str =
    let
        digits : List Int
        digits =
            str
                |> String.split ""
                |> List.map (String.toInt >> Maybe.withDefault -1)

        calculateControl : List Int -> Int
        calculateControl weights =
            weights
                |> List.indexedMap (weighted digits)
                |> List.foldl (+) 0
                |> checkDigit

        control1 =
            digits
                |> List.getAt 9
                |> Maybe.withDefault -1

        control2 =
            digits
                |> List.getAt 10
                |> Maybe.withDefault -1

        calculatedControl1 =
            calculateControl weights1

        calculatedControl2 =
            calculateControl weights2
    in
    if (control1 == calculatedControl1) && (control2 == calculatedControl2) then
        Ok str

    else
        Err InvalidChecksum


{-| Converts a personal number to string representation in the long format that is commonly used for database storage (DDMMYYXXXCC).
-}
toString : PersonalNumber -> String
toString (PersonalNumber str) =
    str


{-| Parse a string into a personal number.
-}
fromString : String -> Result ValidationError PersonalNumber
fromString str =
    let
        pnr =
            String.filter (\c -> List.notMember c [ '-', ' ' ]) <| String.trim str
    in
    case String.length pnr of
        11 ->
            let
                date =
                    String.left 6 pnr

                digits =
                    String.right 5 pnr
            in
            validateFormat (date ++ digits)
                |> Result.andThen validateChecksum
                |> Result.andThen (validateDate InvalidDate)
                |> Result.map PersonalNumber

        _ ->
            Err InvalidLength


{-| Encode a personal number into a JSON value.
-}
encode : PersonalNumber -> Json.Encode.Value
encode pnr =
    Json.Encode.string (toString pnr)


{-| Decode a personal number.
-}
decoder : Json.Decode.Decoder PersonalNumber
decoder =
    let
        decode str =
            case fromString str of
                Ok pnr ->
                    Json.Decode.succeed pnr

                Err _ ->
                    Json.Decode.fail <| "Invalid personal number."
    in
    Json.Decode.string |> Json.Decode.andThen decode


{-| Format a personal number into a user readable string (DDMMYY-XXX-CC).
-}
display : PersonalNumber -> String
display pnr =
    let
        str =
            toString pnr

        date =
            String.left 6 str

        digits =
            String.slice 6 9 str

        control =
            String.right 2 str
    in
    String.join "-" [ date, digits, control ]
