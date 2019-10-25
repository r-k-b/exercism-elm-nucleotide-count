module Tests exposing (tests)

import Expect
import NucleotideCount exposing (nucleotideCounts)
import Test exposing (..)


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
        ]
