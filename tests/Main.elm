port module Main exposing (main, emit)

import Test.Runner.Node exposing (run, TestProgram)
import Test exposing (Test, describe, test)
import Expect
import Json.Encode exposing (Value)
import Murmur3


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg


hashFn : a -> Int
hashFn =
    toString >> Murmur3.hashString 63476


tests : Test
tests =
    describe "Hashing"
        [ test "int" <| \() -> Expect.equal 1992578978 <| hashFn -102433675
        , test "float" <| \() -> Expect.equal 335970363 <| hashFn 4.32
        , test "rec" <| \() -> Expect.equal 3455049611 <| hashFn { name = "Robin", age = "27", male = True }
        , test "tuple" <| \() -> Expect.equal 12752532 <| hashFn ( "Robin", 27, True )
        , test "ls" <| \() -> Expect.equal 4202619459 <| hashFn (List.range 1 6)
        , test "bool" <| \() -> Expect.equal 108766572 <| hashFn False
        ]
