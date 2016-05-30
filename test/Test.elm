module Test.Hash exposing (main)

import ElmTest exposing (..)
import Murmur3


hashFn : String -> Int
hashFn =
    Murmur3.hashString 63476


tests : Test
tests =
    suite "Hashing"
        [ test "int" <| assertEqual 1992578978 <| hashFn <| toString -102433675
        , test "float" <| assertEqual 335970363 <| hashFn <| toString 4.32
        , test "rec" <| assertEqual 3455049611 <| hashFn <| toString { name = "Robin", age = "27", male = True }
        , test "tuple" <| assertEqual 12752532 <| hashFn <| toString ( "Robin", 27, True )
        , test "ls" <| assertEqual 4202619459 <| hashFn <| toString [1..6]
        , test "bool" <| assertEqual 108766572 <| hashFn <| toString False
        ]


main =
    runSuite tests
