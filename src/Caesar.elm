module Caesar exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type Msg
    = Change String


type alias Model =
    { content : String
    }


main =
    Browser.sandbox { init = init, update = update, view = view }


testing : Html msg
testing =
    Html.pre
        []
        (List.map to_div results)


cypher : Char -> Int -> Char
cypher letter offset =
    if Char.toCode letter > Char.toCode 'A' && Char.toCode letter < Char.toCode 'z' then
        Char.fromCode (Char.toCode letter + offset)

    else
        letter


results : List String
results =
    [ "-- Hello-Elm output --\n\n  Cypher calculations:"
    , "-- " ++ return_cyphered_string "rares" 5
    , "-- " ++ return_cyphered_string "ABCD abcd" 5
    , "-- " ++ return_cyphered_string "1234" 5 ++ " -- ignores characters that are not letters"
    , "-- " ++ return_cyphered_string "{}" 5 ++ " -- ignores characters that are not letters"
    , "-- " ++ return_cyphered_string "      1234 \n abcd ;'';/.," 5 ++ " -- properly handles newline characters, tabs, spaces etc."
    , "\n-- end --"
    ]


page_width =
    121


to_wrap : String -> String
to_wrap my_value =
    if String.length my_value <= page_width then
        String.left page_width my_value

    else
        String.left page_width my_value ++ "\n" ++ to_wrap (String.dropLeft page_width my_value)


to_div : String -> Html msg
to_div my_value =
    Html.div [] [ to_wrap my_value |> Html.text ]


init : Model
init =
    { content = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }


apply_cypher : List Char -> Int -> List Char
apply_cypher input_list offset =
    List.map (\x -> cypher x offset) input_list


return_cyphered_string : String -> Int -> String
return_cyphered_string input_string offset =
    String.fromList (apply_cypher (String.toList input_string) offset)


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to cypher", value model.content, onInput Change ] []
        , div [] [ text (return_cyphered_string model.content 5) ]
        , testing
        ]
