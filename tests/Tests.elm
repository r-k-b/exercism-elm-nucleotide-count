module Tests exposing (tests)

import Expect
import NucleotideCount exposing (nucleotideCounts)
import Parser exposing (Problem(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "NucleotideCount"
        [ test "empty dna strand has no nucleotides" <|
            \() ->
                Expect.equal (Result.Ok { a = 0, t = 0, c = 0, g = 0 })
                    (nucleotideCounts "")
        , test "repetitive sequence has only guanine" <|
            \() ->
                Expect.equal (Result.Ok { a = 0, t = 0, c = 0, g = 8 })
                    (nucleotideCounts "GGGGGGGG")
        , test "counts all nucleotides" <|
            \() ->
                Expect.equal (Result.Ok { a = 20, t = 21, c = 12, g = 17 })
                    (nucleotideCounts "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
        , test "returns informative errors for invalid dna" <|
            \() ->
                Expect.equal
                    (Result.Err
                        [ { col = 4, problem = ExpectingSymbol "A", row = 1 }
                        , { col = 4, problem = ExpectingSymbol "C", row = 1 }
                        , { col = 4, problem = ExpectingSymbol "G", row = 1 }
                        , { col = 4, problem = ExpectingSymbol "T", row = 1 }
                        , { col = 4, problem = ExpectingEnd, row = 1 }
                        ]
                    )
                    (nucleotideCounts "ACTDOGGTA")
        ]
