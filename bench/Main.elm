module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark1, benchmark2, benchmark3)
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
            [ benchmark2 "Short string" Murmur3.hashString seed shortStr
            , benchmark2 "Longer string" Murmur3.hashString seed longerStr
            ]
