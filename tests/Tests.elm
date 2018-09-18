port module Tests exposing (hashes)

import Expect
import Murmur3
import Test exposing (Test, describe, test)


hashes : Test
hashes =
    [ ( "Turn me into a hash", 4138100590 )
    , ( "✓ à la mode", 1373299617 )
    , ( "💩💩💩", 2684741067 )
    , ( String.fromInt -102433675, 3067109540 )
    , ( String.fromFloat 4.32, 3411453253 )
    ]
        |> List.map
            (\( input, output ) ->
                test input <|
                    \_ ->
                        Murmur3.hashString 1234 input
                            |> Expect.equal output
            )
        |> describe "Hashing"
