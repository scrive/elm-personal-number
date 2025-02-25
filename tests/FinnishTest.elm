module FinnishTest exposing (suite)

import Expect
import PersonalNumber.Finnish as PersonalNumber
import Test exposing (..)


suite : Test
suite =
    describe "PersonalNumber.Finnish"
        [ describe "fromString"
            [ test "should accept a valid PNR" <|
                \_ ->
                    PersonalNumber.fromString "131052-308T"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "131052-308T")
            , test "should accept a PNR with surrounding whitespace" <|
                \_ ->
                    PersonalNumber.fromString "  111111-111C  "
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "111111-111C")
            , test "should not accept a PNR without the last four digits" <|
                \_ -> Expect.err (PersonalNumber.fromString "200866001386")
            , test "should not accept a PNR with alpha characters not in checknum position" <|
                \_ -> Expect.err (PersonalNumber.fromString "200A66001386")
            , test "should not accept an invalid PNR with the correct format" <|
                \_ -> Expect.err (PersonalNumber.fromString "  - - -- ")
            , test "should not accept a PNR that is not a valid date" <|
                \_ -> Expect.err (PersonalNumber.fromString "300252308T")
            , test "should accept a PNR with a different century marker" <|
                \_ ->
                    PersonalNumber.fromString "010200A9618"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "010200A9618")
            , test "should accept a PNR with a different century marker for those born in or after 2000" <|
                \_ ->
                    PersonalNumber.fromString "010200F9618"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "010200F9618")
            , test "should accept a PNR with a different century marker for those born in the 1900s" <|
                \_ ->
                    PersonalNumber.fromString "010200X9618"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "010200X9618")
            , test "should accept a PNR with a different century marker for those born in the 19th century" <|
                \_ ->
                    PersonalNumber.fromString "010200+9618"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "010200+9618")
            , test "should not accept a PNR with an invalid checksum" <|
                \_ -> Expect.err (PersonalNumber.fromString "131052308X")
            , test "should not accept an empty value" <|
                \_ -> Expect.err (PersonalNumber.fromString "")
            ]

        -- Test a bunch of valid we know are correct
        -- https://www.nets.eu/developer/e-ident/eids/Pages/testusers.aspx#bankidfi
        , describe "display"
            [ test "should display correctly 070770-905D" <|
                \_ ->
                    PersonalNumber.fromString "070770-905D"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "070770-905D")
            , test "should display correctly 291292-918R" <|
                \_ ->
                    PersonalNumber.fromString "291292-918R"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "291292-918R")
            , test "should display correctly 030883-925M" <|
                \_ ->
                    PersonalNumber.fromString "030883-925M"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "030883-925M")
            , test "should display correctly 170677-924F" <|
                \_ ->
                    PersonalNumber.fromString "170677-924F"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "170677-924F")
            , test "should display correctly 010170-999R" <|
                \_ ->
                    PersonalNumber.fromString "010170-999R"
                        |> Result.map PersonalNumber.display
                        |> Expect.equal (Ok "010170-999R")
            ]
        , describe "isMale"
            [ test "should check if personal number is male" <|
                \_ ->
                    PersonalNumber.fromString "111111-111C"
                        |> Result.map PersonalNumber.isMale
                        |> Expect.equal (Ok True)
            ]
        , describe "isFemale"
            [ test "should check if personal number is female" <|
                \_ ->
                    PersonalNumber.fromString "170677-924F"
                        |> Result.map PersonalNumber.isFemale
                        |> Expect.equal (Ok True)
            ]
        , describe "toString"
            [ test "should encode as a string" <|
                \_ ->
                    PersonalNumber.fromString "111111-111C"
                        |> Result.map PersonalNumber.toString
                        |> Expect.equal (Ok "111111-111C")
            ]
        ]
