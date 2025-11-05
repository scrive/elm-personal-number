module DanishTest exposing (suite)

import CompanyNumber.Danish as CompanyNumber
import Expect
import PersonalNumber.Danish as PersonalNumber
import Test exposing (..)


suite : Test
suite =
    describe "Danish"
        [ describe "PersonalNumber"
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
                , test "should not accept an invalid PNR with other characters than digits" <|
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
        , describe "CompanyNumber.Danish"
            [ describe "fromString"
                [ test "should accept a valid CVR 1" <|
                    \_ ->
                        CompanyNumber.fromString "43950932"
                            |> Result.map CompanyNumber.toString
                            |> Expect.equal (Ok "43950932")
                , test "should accept a valid CVR 2" <|
                    \_ ->
                        CompanyNumber.fromString "16662879"
                            |> Result.map CompanyNumber.toString
                            |> Expect.equal (Ok "16662879")
                , test "should accept a CVR with surrounding whitespace" <|
                    \_ ->
                        CompanyNumber.fromString "  00000000\t\n  "
                            |> Result.map CompanyNumber.toString
                            |> Expect.equal (Ok "00000000")
                , test "should ignore dashes and spaces" <|
                    \_ ->
                        CompanyNumber.fromString "00-00 00-00"
                            |> Result.map CompanyNumber.toString
                            |> Expect.equal (Ok "00000000")
                , test "should not accept a CVR with less than 8 digits" <|
                    \_ -> Expect.err (CompanyNumber.fromString "1234567")
                , test "should not accept an invalid CVR with more than 8 digits" <|
                    \_ -> Expect.err (CompanyNumber.fromString "123456789")
                , test "should not accept an empty value" <|
                    \_ -> Expect.err (CompanyNumber.fromString "")
                , test "should not accept letters" <|
                    \_ -> Expect.err (CompanyNumber.fromString "a00000000")
                , test "should not accept special characters" <|
                    \_ -> Expect.err (CompanyNumber.fromString "_00000000")
                , test "should not accept invalid checksum" <|
                    \_ -> Expect.err (CompanyNumber.fromString "00000001")
                ]
            , describe "display"
                [ test "should display correctly 43950932" <|
                    \_ ->
                        CompanyNumber.fromString "43950932"
                            |> Result.map CompanyNumber.display
                            |> Expect.equal (Ok "43950932")
                , test "should display correctly 1666-2879" <|
                    \_ ->
                        CompanyNumber.fromString " 1666-2879 "
                            |> Result.map CompanyNumber.display
                            |> Expect.equal (Ok "16662879")
                ]
            , describe "toString"
                [ test "should encode as a string" <|
                    \_ ->
                        CompanyNumber.fromString " 4395-0932 "
                            |> Result.map CompanyNumber.toString
                            |> Expect.equal (Ok "43950932")
                ]
            ]
        ]
