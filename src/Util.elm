module Util exposing (char, digit, getSerialPart, stringLength, validateDate, whitespace)

import List.Extra as List
import Parser exposing ((|.), Parser, Step(..), chompIf, chompWhile, getChompedString)


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')


digit : Parser ()
digit =
    chompIf Char.isDigit


char : Parser ()
char =
    chompIf Char.isAlphaNum


stringLength : Int -> Parser String
stringLength =
    getChompedString
        << Parser.loop 0
        << helper


helper : Int -> Int -> Parser (Step Int Int)
helper length count =
    if length == count then
        Parser.succeed () |> Parser.map (always <| Done count)

    else
        Parser.succeed (Loop (count + 1))
            |. chompIf (always True)


getSerialPart : String -> String
getSerialPart =
    String.slice 7 10


maxDaysInMonth : List Int
maxDaysInMonth =
    [ 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


validateDayAndMonth : Int -> Int -> Bool
validateDayAndMonth day month =
    let
        maxDays =
            maxDaysInMonth
                |> List.getAt month
                |> Maybe.withDefault 32
    in
    if month < 1 || month > 12 || day < 1 || day > maxDays then
        False

    else
        True


validateDate : e -> String -> Result e String
validateDate invalidDate str =
    let
        day =
            str
                |> String.left 2
                |> String.toInt
                |> Maybe.withDefault -1

        month =
            str
                |> String.slice 2 4
                |> String.toInt
                |> Maybe.withDefault -1

        isValid =
            -- FH-number
            (day >= 80)
                -- D-number
                || (day >= 40 && validateDayAndMonth (day - 40) month)
                -- H-number
                || (month >= 40 && validateDayAndMonth day (month - 40))
                -- Birth number
                || validateDayAndMonth day month
    in
    if isValid then
        Ok str

    else
        Err invalidDate
