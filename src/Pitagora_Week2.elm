module Pitagora_Week2 exposing (..)
import Browser
import Array
import List
import Debug exposing (log)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


isTriple: Int -> Int -> Int -> Bool
isTriple x y z =
    if ( x == 0 || y == 0 || z == 0 || x < 0 || y < 0 || z < 0) then
        False
    else
    if (x ^ 2 + y ^ 2 == z ^ 2) then
        True
    else
        if (z ^ 2 + y ^ 2 == x ^ 2) then
            True
        else
            if (x ^ 2 + z ^ 2 == y ^ 2) then
                True
            else
                False

leg1 : Int -> Int -> Int
leg1 x y =
    x^2 - y^2

leg2 : Int -> Int -> Int
leg2 x y =
    2*x*y

hyp : Int -> Int -> Int
hyp x y =
    x^2 + y^2

pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) =
    let n1 = leg1 x y in
        let n2 = leg2 x y in
            let n3 = hyp x y in
                (n1, n2, n3)

isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (x,y,z) =
    isTriple x y z

pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap list =
    List.map pythTriple list

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter list =
    List.filter isTripleTuple list

pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec list =
    case list of
        [] -> []
        x::xs ->
            pythTriple x::pythTriplesRec xs

arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec list =
    case list of
        [] -> []
        x::xs ->
            if (isTripleTuple x) then
                x::arePythTriplesRec xs
            else
                arePythTriplesRec xs

var = pythTriplesMap [(5,4),(2,1),(35,7)]
var2 = arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]
var3 = pythTriplesRec [(5,4),(2,1),(35,7)]
var4 = arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (100,2,500), (6,8,10), (7,2,2)]

range = List.range 1 5
my_results =
    [
        "pythTriplesMap [(5,4),(2,1),(35,7)]:",
        Debug.toString var,
        "arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]:",
        Debug.toString var2,
        "pythTriplesRec [(5,4),(2,1),(35,7)]:",
        Debug.toString var3,
        "arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (100,2,500), (6,8,10), (7,2,2)]:",
        Debug.toString var4
    ]

-- create main method (Boiler-plate)

to_div my_value =
    Html.div [] [ my_value |> Html.text ]

main = Html.div
        []
        (List.map to_div my_results)
