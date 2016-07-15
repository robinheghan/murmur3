module Bench.Murmur3 exposing (benchmarks)

import Bench.Native exposing (BenchmarkSuite, bench, suite)
import Murmur3


hashFn : String -> () -> Int
hashFn str =
    \() -> Murmur3.hashString 63476 str


benchmarks : BenchmarkSuite
benchmarks =
    suite "Murmur3"
        [ bench "String key" <| hashFn "DictKey" ]
