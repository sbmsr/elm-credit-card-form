module Helpers exposing ( isValidCreditCardNumber )

import String exposing (fromInt, length)

import Constants exposing (..)

toListOfInt : List String -> Result String (List Int)
toListOfInt strings =
    strings
        |> List.reverse
        |> List.foldl
            (\numberString acc ->
                case String.toInt numberString of
                    Just num ->
                        Result.map ((::) num) acc

                    Nothing ->
                        Err <| "Not a number: " ++ numberString
            )
            (Ok [])


doubleAtEvenIndices : List Int -> List Int
doubleAtEvenIndices =
    List.indexedMap
        (\index num ->
            if modBy 2 index == 0 then
                num * 2

            else
                num
        )


keepUnder10 : Int -> Int
keepUnder10 num =
    if num >= 10 then
        num - 9

    else
        num


checksum : Int -> Int -> Result String Bool
checksum checkDigit sum =
    if modBy 10 (sum + checkDigit) == 0 then
        Ok True

    else
        Err "Incorrect checksum"


notEmpty : List a -> Result String (List a)
notEmpty nums =
    if List.isEmpty nums then
        Err "Input string is only check digit"

    else
        Ok nums

-- LUHN ALGORITHM

luhn : String -> Result String String
luhn numberString =
    let
        numbers =
            numberString
                |> String.split ""
                |> List.reverse
                |> toListOfInt

        checkDigit =
            numbers
                |> Result.andThen
                    (Result.fromMaybe "Input string is empty" << List.head)
                |> Result.withDefault 0
    in
    numbers
        |> Result.andThen (Result.fromMaybe "Input string is empty" << List.tail)
        |> Result.andThen notEmpty
        |> Result.map doubleAtEvenIndices
        |> Result.map (List.map keepUnder10)
        |> Result.map List.sum
        |> Result.andThen (checksum checkDigit)
        |> Result.map (\_ -> numberString)

isValidCreditCardNumber : String -> Bool
isValidCreditCardNumber numberString =
    case luhn numberString of
        Ok _ ->
            (length numberString) == ccLength

        Err _ ->
            False