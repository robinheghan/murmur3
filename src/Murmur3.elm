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
        |> String.foldr (\c acc -> Char.toCode c :: acc) []
        |> hashChars seed
        |> finalize (String.length str)


hashChars : Int -> List Int -> Int
hashChars seed codes =
    if List.isEmpty codes then
        seed
    else
        let
            rest =
                List.drop 4 codes

            hash =
                hashFourChars codes 0
                    |> mix seed
        in
            -- Only step if we hashed a full set of characters
            if greaterThanFour 0 codes then
                hashChars (step hash) rest
            else
                hashChars hash rest


hashFourChars : List Int -> Int -> Int
hashFourChars codes shift =
    if shift >= 32 then
        0
    else
        case codes of
            x :: xs ->
                ((x `and` 0xFF) `shiftLeft` shift) `or` hashFourChars xs (shift + 8)

            _ ->
                0


greaterThanFour : Int -> List Int -> Bool
greaterThanFour num codes =
    if num >= 4 then
        True
    else
        case codes of
            x :: xs ->
                greaterThanFour (num + 1) xs

            [] ->
                False


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


finalize : Int -> Int -> Int
finalize strLength acc =
    let
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
