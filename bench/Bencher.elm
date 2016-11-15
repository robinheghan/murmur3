module Main exposing (main)

import Html
import Bench.Native as Benchmark
import Bench.Murmur3


main : Program Never () msg
main =
    Html.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Done!"
        }
        |> Benchmark.run
            [ Bench.Murmur3.benchmarks
            ]
