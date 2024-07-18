module DanishTest exposing (suite)

import Expect
import PersonalNumber.Danish as PersonalNumber
import Test exposing (..)


suite : Test
suite =
    describe "PersonalNumber.Danish"
        [ describe "fromString"
            [ test "should accept a valid PNR" <|
                \_ ->
                    PersonalNumber.fromString "160688-4523"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "160688-4523")
            , test "should accept a PNR with surrounding whitespace" <|
                \_ ->
                    PersonalNumber.fromString "  160688-4523  "
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "160688-4523")
            , test "should not accept a PNR with less than 10 digits" <|
                \_ -> Expect.err (PersonalNumber.fromString "160688")
            , skip <|
                test "should not accept an invalid PNR with other characters than digits" <|
                    \_ -> Expect.err (PersonalNumber.fromString "160688-45MM")
            , test "should not accept a PNR that is not a valid date" <|
                \_ -> Expect.err (PersonalNumber.fromString "3206884523")
            , test "should not accept an empty value" <|
                \_ -> Expect.err (PersonalNumber.fromString "")
            ]
        , describe "display"
            [ test "should display correctly 160688-4523" <|
                \_ ->
                    PersonalNumber.fromString "160688-4523"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "160688-4523")
            , test "should display correctly 1606884523" <|
                \_ ->
                    PersonalNumber.fromString "1606884523"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "1606884523")
            ]
        , describe "toString"
            [ test "should encode as a string" <|
                \_ ->
                    PersonalNumber.fromString "160688-4523"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "160688-4523")
            ]
        ]
