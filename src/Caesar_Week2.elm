module Caesar_Week2 exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html exposing (a)



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

-- Render HTML results
page_width =
    121

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

results : List String
results =
    [ "-- Hello-Elm output --\n\n  Cypher calculations:"
    , stringFromBool (isLetter 'a')
    , stringFromBool (isLetter 'Z')
    , stringFromBool (isLetter 'z')
    , stringFromBool (isLetter 'A')
    , stringFromBool (isLetter ' ')
    , stringFromBool (isLetter '_')
    , stringFromBool (isLetter '[')
    , stringFromBool (isLetter '@')
    , normalize "Hello, Fontys!@#="
    , "should look like HelloFontys"
    , String.concat (encrypt 7 (String.toList (normalize "Hello, Fontys!"))) -- outputs fghiyjxyklm
    , String.concat (decrypt 7 (String.toList "OlssvMvuafz"))


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
        [ input [ placeholder "Text to cypher", value model.content, onInput Change ] []
        , div [] [ text (String.concat (encrypt 7 (String.toList (normalize model.content)))) ]
        , testtingModule
        ]
