module Caesar_Week3 exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html exposing (a)
import Html.Events exposing (keyCode)
import List.Extra exposing (unique)



cypher : Char -> Int -> Char
cypher letter offset =
    if ((Char.toCode letter + offset) > Char.toCode 'Z' && (Char.toCode letter + offset) < Char.toCode 'a') || (Char.toCode letter + offset) > Char.toCode 'z' then
        Char.fromCode (Char.toCode letter + offset - 26)
    else 
         Char.fromCode (Char.toCode letter + offset)



decypher : Char -> Int -> Char
decypher letter offset =
    if ((Char.toCode letter - offset) >= Char.toCode 'Z' && (Char.toCode letter - offset) < Char.toCode 'a') || (Char.toCode letter - offset) < Char.toCode 'A' then
        Char.fromCode (Char.toCode letter - offset + 26)
    else 
        Char.fromCode (Char.toCode letter - offset)


isLetter : Char -> Bool
isLetter letter =
    if (Char.toCode letter >= Char.toCode 'A' && Char.toCode letter <= Char.toCode 'Z') || (Char.toCode letter >= Char.toCode 'a' && Char.toCode letter <= Char.toCode 'z') then
        True
    else
        False

normalize: String -> String
normalize input_string = 
    String.fromList (List.filter isLetter (String.toList(input_string)))


encrypt : Int -> List Char -> List String
encrypt offset input_string = 
    case input_string of
        [] -> []
        x :: xs -> (String.fromChar (cypher x offset)) :: (encrypt offset xs) 

decrypt: Int -> List Char -> List String
decrypt offset input_string =
    case input_string of
        [] -> []
        x :: xs -> (String.fromChar (decypher x offset)) :: (decrypt offset xs) 


encode_string: Int -> String -> String
encode_string offset input_string = 
    String.concat (encrypt offset (String.toList (normalize input_string)))


decode_string: Int -> String -> String
decode_string offset input_string = 
    String.concat (decrypt offset (String.toList input_string))


decode_all_keys: List Int -> String -> List (Int,String)
decode_all_keys key_list input_string = 
    case key_list of
        [] -> []
        x :: xs -> Tuple.pair x ("Key: " ++ String.fromInt x ++ " " ++ (decode_string x input_string) ++ "\n") :: decode_all_keys xs input_string

candidates: List String -> List (Int,String) -> List (Int,String)
candidates words decoded_strings = 
    case words of
    [] -> []
    z::zs -> 
         (List.foldr(\x result -> if String.contains z (Tuple.second x) then x::result else result) [] (List.foldr (\y result -> y :: result) [] decoded_strings)) ++ candidates zs decoded_strings

-- Render HTML results
page_width =
    1500

init : Model
init =
    { content = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }

to_wrap : String -> String
to_wrap my_value =
    if String.length my_value <= page_width then
        String.left page_width my_value

    else
        String.left page_width my_value ++ "\n" ++ to_wrap (String.dropLeft page_width my_value)

to_div : String -> Html msg
to_div my_value =
    Html.div [] [ to_wrap my_value |> Html.text ]

type Msg
    = Change String


type alias Model =
    { content : String
    }


main =
    Browser.sandbox { init = init, update = update, view = view }

stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "True"

  else
    "False"
popular_words = ["the", "at", "some", "my", "this", "have", "been", "an", "go"]
results : List String
results =
    [ "-- Hello-Elm output --\n\n  Cypher calculations:"
    , "Input String: DGGADBCOOCZYMJHZYVMTOJOCZHVS"
    ,  String.concat (List.foldr (\x result -> Tuple.second x::result) [] (decode_all_keys (List.range 1 25) "DGGADBCOOCZYMJHZYVMTOJOCZHVS"))
    ,  "\n Candidate results:"
    ,  String.concat (List.foldr (\x result -> Tuple.second x::result) [] (candidates ["THE", "AND"] (decode_all_keys (List.range 1 25) "DGGADBCOOCZYMJHZYVMTOJOCZHVS")))
    ,  "\n Test String: Testing the candidates feature"
    ,  " Encoded: " ++ encode_string 11 "Testing the candidates feature"
    ,  " All possible decodings: \n" ++ String.concat (List.foldr (\x result -> Tuple.second x::result) [] (decode_all_keys (List.range 1 25) "Epdetyrespnlyotolepdqplefcp"))
    ,  "Found candidates: \n" ++ String.concat (List.foldr (\x result -> Tuple.second x::result) [] (unique (candidates popular_words (decode_all_keys (List.range 1 25) "Epdetyrespnlyotolepdqplefcp"))))
    , "\n-- end --"
    ]

testtingModule : Html msg
testtingModule =
    Html.pre
        []
        (List.map to_div results)

        
view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to cypher with offset 7", value model.content, onInput Change ] []
        , div [] [ text (encode_string 7 model.content) ]
        , testtingModule
        ]
