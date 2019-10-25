module NucleotideCount exposing (nucleotideCounts)

import Parser exposing ((|=), Parser, Step, map, symbol)


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


nucleotideCounts : String -> Result (List Parser.DeadEnd) NucleotideCounts
nucleotideCounts sequence =
    sequence
        |> Parser.run decodeDNA
        |> Result.map (List.foldl count allZeros)


allZeros =
    { a = 0
    , c = 0
    , g = 0
    , t = 0
    }


decodeDNA : Parser DNA
decodeDNA =
    Parser.loop [] decodeDNAhelp


type alias DNA =
    List Nucleotide


decodeDNAhelp : DNA -> Parser (Step DNA DNA)
decodeDNAhelp dna =
    Parser.oneOf
        [ Parser.succeed (\nucleotide -> Parser.Loop (nucleotide :: dna))
            |= decodeNucleotide
        , Parser.end
            |> map (\_ -> Parser.Done (List.reverse dna))
        ]


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
