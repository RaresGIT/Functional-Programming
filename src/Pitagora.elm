module Pitagora exposing (..)
import Browser
import Array
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


sqr: Int -> Int
sqr x =
        x^2

isTriple: Int -> Int -> Int -> Bool
isTriple x y z =
    -- let my_list = [x, y, z]
    -- sort my_list
    -- let array = List.sort [x,y,z] in
    -- let new_list = Array.fromList array in
    --     if (Array.get new_list ^ 2 + new_list[1] ^ 2 == new_list[2] ^ 2) then
    --         True
    --     else
    --         False
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

first : ( Int, Int, Int ) -> Int
first (x,y,z) = 
    x

second : ( Int, Int, Int ) -> Int
second (x,y,z) = 
    y

third : ( Int, Int, Int ) -> Int
third (x,y,z) = 
    z

stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "True"

  else
    "False"

isTripleTuple: (Int, Int, Int) -> Bool 
isTripleTuple (x,y,z) = 
    isTriple x y z

var = sqr 4
leg1_var = leg1 5 4
leg2_var = leg2 5 4
hyp_var = hyp 5 4   
isTriple_var = isTriple 9 40 41
pythTriple_var = pythTriple (5,4)
pythTriple_var_first = first pythTriple_var
pythTriple_var_second = second pythTriple_var
pythTriple_var_third = third pythTriple_var
isTripleTuple_var = isTripleTuple (9,40,41)
my_results =
    [
        "Sqr of 4:",
       String.fromInt var,
        "Leg1 of 5 4:",
        String.fromInt leg1_var,
        "Leg 2 of 5 4:",
        String.fromInt leg2_var,
        "Hyp of 5 4:",
        String.fromInt hyp_var,
        "Is triple for 9 40 41",
        stringFromBool isTriple_var,
        "pythTriple for 5 4",
        String.fromInt pythTriple_var_first ++ " " ++ String.fromInt pythTriple_var_second ++ " " ++ String.fromInt pythTriple_var_third,
        "is triple tuple for 9 40 41",
        stringFromBool isTripleTuple_var

        
    ] 
    
-- create main method (Boiler-plate)

to_div my_value = 
    Html.div [] [ my_value |> Html.text ]

main = Html.div 
        []
        (List.map to_div my_results)
