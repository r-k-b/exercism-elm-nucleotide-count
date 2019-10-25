module NucleotideCount exposing (nucleotideCounts)

import Parser exposing ((|=), Parser, map, symbol)


type Nucleotide
    = Adenine
    | Cytosine
    | Guanine
    | Thymine


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    case sequence |> Parser.run decodeDNA of
        Ok dna ->
            dna |> List.foldl count allZeros

        Err _ ->
            allZeros


allZeros =
    { a = 0
    , c = 0
    , g = 0
    , t = 0
    }


decodeDNA : Parser (List Nucleotide)
decodeDNA =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = decodeNucleotide
        , trailing = Parser.Optional
        }


decodeNucleotide : Parser Nucleotide
decodeNucleotide =
    Parser.oneOf
        [ symbol "A" |> map (\_ -> Adenine)
        , symbol "C" |> map (\_ -> Cytosine)
        , symbol "G" |> map (\_ -> Guanine)
        , symbol "T" |> map (\_ -> Thymine)
        ]


count : Nucleotide -> NucleotideCounts -> NucleotideCounts
count nucleotide dna =
    case nucleotide of
        Adenine ->
            { dna | a = dna.a + 1 }

        Cytosine ->
            { dna | c = dna.c + 1 }

        Guanine ->
            { dna | g = dna.g + 1 }

        Thymine ->
            { dna | t = dna.t + 1 }
