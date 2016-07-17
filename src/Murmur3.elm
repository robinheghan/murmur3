module Murmur3 exposing (hashString)

{-| Murmur 3 hash function for hashing strings

@docs hashString

-}

import Bitwise exposing (..)
import String
import Char


{-| Takes a seed and a string. Produces a hash (integer).
Given the same seed and string, it will always produce the same hash.

    hashString 1234 "Turn me into a hash" == 4138100590
-}
hashString : Int -> String -> Int
hashString seed str =
    str
        |> String.foldl hashFold ( 0, seed, 0 )
        |> finalize (String.length str)


hashFold : Char -> ( Int, Int, Int ) -> ( Int, Int, Int )
hashFold c ( shift, seed, hash ) =
    let
        res =
            (((Char.toCode c) `and` 0xFF) `shiftLeft` shift) `or` hash
    in
        if shift >= 24 then
            let
                newHash =
                    res |> mix seed |> step
            in
                ( 0, newHash, 0 )
        else
            ( shift + 8, seed, res )


finalize : Int -> ( Int, Int, Int ) -> Int
finalize strLength ( _, seed, hash ) =
    let
        acc =
            if hash /= 0 then
                mix seed hash
            else
                seed

        h1 =
            acc `Bitwise.xor` strLength

        h2 =
            h1
                `Bitwise.xor` (h1 `shiftRightLogical` 16)
                |> mur 0x85EBCA6B

        h3 =
            h2
                `Bitwise.xor` (h2 `shiftRightLogical` 13)
                |> mur 0xC2B2AE35

        h4 =
            h3 `Bitwise.xor` (h3 `shiftRightLogical` 16)
    in
        h4 `shiftRightLogical` 0


mix : Int -> Int -> Int
mix h1 h2 =
    let
        k1 =
            mur 0xCC9E2D51 h2

        k2 =
            (k1 `shiftLeft` 15)
                `or` (k1 `shiftRightLogical` 17)
                |> mur 0x1B873593
    in
        h1 `Bitwise.xor` k2


mur : Int -> Int -> Int
mur c h =
    (((h `and` 0xFFFF) * c) + ((((h `shiftRightLogical` 16) * c) `and` 0xFFFF) `shiftLeft` 16)) `and` 0xFFFFFFFF


step : Int -> Int
step acc =
    let
        h1 =
            (acc `shiftLeft` 13)
                `or` (acc `shiftRightLogical` 19)
                |> mur 5
    in
        ((h1 `and` 0xFFFF) + 0x6B64) + ((((h1 `shiftRightLogical` 16) + 0xE654) `and` 0xFFFF) `shiftLeft` 16)
