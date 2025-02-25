module PersonalNumber.Finnish exposing
    ( PersonalNumber, isFemale, isMale
    , ValidationError(..)
    , fromString, toString, display
    , decoder, encode
    )

{-| Parse Finnish personal numbers (Finnish: henkilötunnus, hetu in short), are used to
identify citizens. Hetu PIN consists of eleven characters: DDMMYYCZZZQ,
where DDMMYY is the day, month and year of birth, C is the century marker,
ZZZ is the individual number and Q is the control character.


# Definition

@docs PersonalNumber, isFemale, isMale


# Errors

@docs ValidationError


# Strings

@docs fromString, toString, display


# JSON

@docs decoder, encode

-}

import Basics.Extra exposing (flip)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import Parser exposing ((|.), Parser, chompIf, getChompedString)
import Util exposing (char, digit, getSerialPart, validateDate, whitespace)


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



--- CONSTANTS


checksumChars : String
checksumChars =
    "0123456789ABCDEFHJKLMNPRSTUVWXY"


{-| Separators used in personal identity codes:

  - for those born in or after 2000, letters A, B, C, D, E, F.
  - for those born in the 1900s, the current hyphen (-) or the letters Y, X, W, V, U.
  - for those born in the 19th century, a plus sign (+).

-}
separatorChars : String
separatorChars =
    "-+ABCDEFYXWVU"


personalNumberParser : Parser PersonalNumber
personalNumberParser =
    (getChompedString <|
        Parser.succeed ()
            |. whitespace
            |. digit
            |. digit
            |. digit
            |. digit
            |. digit
            |. digit
            |. chompIf (String.fromChar >> flip String.contains separatorChars)
            |. digit
            |. digit
            |. digit
            -- checksum
            |. char
    )
        |> Parser.map PersonalNumber


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
        datePart : String
        datePart =
            String.left 6 str

        checksumCharacter : String
        checksumCharacter =
            String.right 1 str

        computedChecksumIdx : Maybe Int
        computedChecksumIdx =
            (String.toInt <| datePart ++ getSerialPart str)
                |> Maybe.map (modBy <| String.length checksumChars)

        checkDigit : Maybe String
        checkDigit =
            computedChecksumIdx
                |> Maybe.andThen (flip List.getAt (String.split "" checksumChars))
    in
    if Just checksumCharacter == checkDigit then
        Ok str

    else
        Err InvalidChecksum


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


{-| Converts a personal number to string representation in the long format that is
commonly used for database storage (DDMMYYCZZZQ).
-}
toString : PersonalNumber -> String
toString (PersonalNumber str) =
    str


{-| Parse a string into a personal number.
-}
fromString : String -> Result ValidationError PersonalNumber
fromString str =
    let
        pnr : String
        pnr =
            String.trim str
    in
    case String.length pnr of
        11 ->
            validateFormat pnr
                |> Result.andThen validateChecksum
                |> Result.andThen (validateDate InvalidDate)
                |> Result.map PersonalNumber

        _ ->
            Err InvalidLength


{-| Encode a personal number into a JSON value.
-}
encode : PersonalNumber -> Encode.Value
encode =
    Encode.string << toString


{-| Decode a personal number.
-}
decoder : Decoder PersonalNumber
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


{-| Format a personal number into a user readable string (DDMMYY-ZZZQ).
-}
display : PersonalNumber -> String
display =
    toString
