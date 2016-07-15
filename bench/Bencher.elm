module Main exposing (main)

import Html
import Html.App
import Bench.Native as Benchmark
import Bench.Murmur3


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Done!"
        }
        |> Benchmark.run
            [ Bench.Murmur3.benchmarks
            ]
