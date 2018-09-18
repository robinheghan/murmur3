module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Murmur3


seed : Int
seed =
    63476


shortStr : String
shortStr =
    "DictKey"


longerStr : String
longerStr =
    "This is a longer string, with a comma and everything."


main : BenchmarkProgram
main =
    program <|
        describe "Murmur3"
            [ benchmark "short string" (\_ -> Murmur3.hashString seed shortStr)
            , benchmark "longer string" (\_ -> Murmur3.hashString seed longerStr)
            ]
