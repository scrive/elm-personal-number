module CompanyNumber.Danish exposing
    ( CompanyNumber
    , ValidationError(..)
    , fromString, toString, display
    , decoder, encode
    )

{-| Parse Danish company numbers (CVR).
CVR consists of eight numbers satisfying a mod-11 check.

Exact CVR format specification: <https://hl7.dk/fhir/core/2.2.0/StructureDefinition-dk-core-cvr-identifier.html>


# Definition

@docs CompanyNumber


# Errors

@docs ValidationError


# Strings

@docs fromString, toString, display


# JSON

@docs decoder, encode

-}

import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Parser exposing ((|.), Parser, getChompedString)


{-| An opaque type representing a valid personal number.
-}
type CompanyNumber
    = CompanyNumber String


{-| If the parsing fails an error of this type is returned.
-}
type ValidationError
    = InvalidLength
    | InvalidFormat
    | InvalidChecksum


companyNumberParser : Parser CompanyNumber
companyNumberParser =
    (getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> Char.isDigit c)
    )
        |> Parser.map CompanyNumber


validateFormat : String -> Result ValidationError String
validateFormat str =
    case Parser.run companyNumberParser str of
        Ok pnr ->
            Ok (toString pnr)

        Err _ ->
            Err InvalidFormat


validateChecksum : String -> Result ValidationError String
validateChecksum str =
    let
        checksum =
            String.split "" str
                |> List.map (String.toInt >> Maybe.withDefault -1)
                |> List.zip [ 2, 7, 6, 5, 4, 3, 2, 1 ]
                |> List.map (\( a, b ) -> a * b)
                |> List.foldl (+) 0
                |> modBy 11
    in
    if checksum == 0 then
        Ok str

    else
        Err InvalidChecksum


{-| Parse a string into a company number.
-}
fromString : String -> Result ValidationError CompanyNumber
fromString str =
    let
        pnr : String
        pnr =
            String.filter (\c -> List.notMember c [ '-', ' ' ]) <| String.trim str
    in
    case String.length pnr of
        8 ->
            validateFormat pnr
                |> Result.andThen validateChecksum
                |> Result.map CompanyNumber

        _ ->
            Err InvalidLength


{-| Converts a company number to string representation in the long format that is
commonly used for database storage (XXXXXXXX).
-}
toString : CompanyNumber -> String
toString (CompanyNumber str) =
    str


{-| Encode a company number into a JSON value.
-}
encode : CompanyNumber -> Encode.Value
encode =
    Encode.string << toString


{-| Decode a company number.
-}
decoder : Decode.Decoder CompanyNumber
decoder =
    let
        decode str =
            case fromString str of
                Ok pnr ->
                    Decode.succeed pnr

                Err _ ->
                    Decode.fail "Invalid company number."
    in
    Decode.string |> Decode.andThen decode


{-| Format a company number into a user readable string (XXXXXXXX).
-}
display : CompanyNumber -> String
display =
    toString
