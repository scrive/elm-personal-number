module PersonalNumber.Swedish exposing
    ( PersonalNumber, isPNR, isSAM
    , ValidationError(..)
    , fromString, toString, display
    , decoder, encode
    )

{-| Parse Swedish personal numbers.

Supports both ordinary "personnummer" and "sammordningsnummer" assigned to foreign
temporary visitors.


# Definition

@docs PersonalNumber, isPNR, isSAM


# Errors

@docs ValidationError


# Strings

@docs fromString, toString, display


# JSON

@docs decoder, encode

-}

import Json.Decode
import Json.Encode
import Parser exposing ((|.), Parser, getChompedString)
import String
import Util exposing (stringLength, whitespace)


{-| An opaque type representing a valid personal number.
-}
type PersonalNumber
    = PNR String
    | SAM String


{-| Check if the personal number is a "personnummer".
-}
isPNR : PersonalNumber -> Bool
isPNR pnr =
    case pnr of
        PNR _ ->
            True

        _ ->
            False


{-| Check if the personal number is a "sammordningsnummer".
-}
isSAM : PersonalNumber -> Bool
isSAM pnr =
    case pnr of
        SAM _ ->
            True

        _ ->
            False


{-| If the parsing fails an error of this type is returned.
-}
type ValidationError
    = InvalidFormat
    | InvalidLength
    | InvalidDate
    | InvalidChecksum


personalNumberParser : Parser PersonalNumber
personalNumberParser =
    (getChompedString <|
        Parser.succeed ()
            |. whitespace
            |. stringLength 8
            |. stringLength 4
    )
        |> Parser.map PNR


validateFormat : String -> Result ValidationError String
validateFormat str =
    case Parser.run personalNumberParser str of
        Ok pnr ->
            Ok (toString pnr)

        Err _ ->
            Err InvalidFormat


verifyChecksum : String -> Result ValidationError String
verifyChecksum str =
    let
        checksum =
            str
                |> String.split ""
                |> List.reverse
                |> List.take 10
                |> List.reverse
                |> List.map String.toInt
                |> List.map (Maybe.withDefault -1)
                |> List.indexedMap (\a b -> ( a, b ))
                |> List.map
                    (\( i, a ) ->
                        if modBy 2 i == 0 then
                            a * 2

                        else
                            a
                    )
                |> List.map
                    (\a ->
                        if a > 9 then
                            1 + (a - 10)

                        else
                            a
                    )
                |> List.foldl (+) 0
    in
    if modBy 10 checksum == 0 then
        Ok str

    else
        Err InvalidChecksum


numberType : String -> Result ValidationError PersonalNumber
numberType str =
    let
        year =
            String.slice 0 4 str |> String.toInt |> Maybe.withDefault 0

        month =
            String.slice 4 6 str |> String.toInt |> Maybe.withDefault 0

        day =
            String.slice 6 8 str |> String.toInt |> Maybe.withDefault 0
    in
    if year >= 1900 && month >= 1 && month <= 12 then
        if day >= 1 && day <= 31 then
            Ok (PNR str)

        else if day >= 61 && day <= 91 then
            Ok (SAM str)

        else
            Err InvalidDate

    else
        Err InvalidDate


{-| Parse a string into a personal number.
-}
fromString : String -> Result ValidationError PersonalNumber
fromString str =
    let
        pnr =
            String.trim <| String.filter (\c -> c /= '-') str
    in
    case String.length pnr of
        10 ->
            if
                String.startsWith "0" pnr
                    || String.startsWith "1" pnr
            then
                fromString ("20" ++ pnr)

            else
                fromString ("19" ++ pnr)

        12 ->
            let
                date =
                    String.left 8 pnr

                digits =
                    String.right 4 pnr
            in
            validateFormat (date ++ digits)
                |> Result.andThen verifyChecksum
                |> Result.andThen numberType

        _ ->
            Err InvalidLength


{-| Converts a personal number to string representation in the long format that is commonly used for database storage (YYYYMMDDXXXX).
-}
toString : PersonalNumber -> String
toString pnr =
    case pnr of
        PNR str ->
            str

        SAM str ->
            str


{-| Encode a personal number into a JSON value.
-}
encode : PersonalNumber -> Json.Encode.Value
encode =
    Json.Encode.string << toString


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
                    Json.Decode.fail "Invalid personal number."
    in
    Json.Decode.string |> Json.Decode.andThen decode


{-| Format a personal number into a user readable string (YYYYMMDD-XXXX).
-}
display : PersonalNumber -> String
display pnr =
    let
        str =
            toString pnr

        date =
            String.left 8 str

        digits =
            String.right 4 str
    in
    String.join "-" [ date, digits ]
