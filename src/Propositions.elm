module Propositions exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

type alias Variable = String

type Proposition
    = Or Proposition Proposition
    | And Proposition Proposition
    | Impl Proposition Proposition
    | Bi Proposition Proposition
    | Not Proposition
    | Var Variable


print: Proposition -> String
print inputProposition = 
    Debug.toString inputProposition

parse: Proposition -> String
parse input = 
    case input of
        Or prop prop1 -> "(" ++ (parse prop) ++ " or " ++ (parse prop1) ++ ")"
        Not prop-> "(" ++ "! " ++ (parse prop) ++ ")"
        And prop prop1 ->  "(" ++ (parse prop) ++ " and " ++  (parse prop1) ++ ")"
        Impl prop prop1 ->  "(" ++ (parse prop) ++ " => " ++  (parse prop1) ++ ")"
        Bi prop prop1 ->  "(" ++ (parse prop) ++ " <=> " ++  (parse prop1) ++ ")"
        Var string ->  string

switcheroo: Proposition -> String
switcheroo input = 
    case input of
        Or prop prop1 -> "(" ++ (switcheroo prop) ++ " or " ++ (switcheroo prop1) ++ ")"
        Not prop-> "(" ++ "! " ++ (switcheroo prop) ++ ")"
        And prop prop1 ->  "(" ++ (switcheroo prop) ++ " and " ++  (switcheroo prop1) ++ ")"
        Impl prop prop1 ->  "(" ++ "!" ++ (switcheroo prop) ++ " or " ++  (switcheroo prop1) ++ ")"
        Bi prop prop1 ->  "(" ++ (switcheroo prop) ++ " or " ++  "not " ++ (switcheroo prop1) ++ ")"
        Var string ->  string
results : List String
results =
    [ "-- Hello-Elm output --\n\n  Cypher calculations:",
    --   print (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
      parse (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
      parse (Impl (Var "B") (Var "C")),
      parse (Or (Impl (Var "B") (Not (Var "C"))) (Bi (Not (Var "D")) (Var "E"))),
      "\n",
      "-- SWITCHEROO --\n",
      switcheroo (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
      switcheroo (Impl (Var "B") (Var "C")),
      switcheroo (Or (Impl (Var "B") (Not (Var "C"))) (Bi (Not (Var "D")) (Var "E"))),
      "\n-- end --"
    ]



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


view : Model -> Html Msg
view model =
    div []
        -- [ input [ placeholder "Text to cypher", value model.content, onInput Change ] []
        [
        testing
        ]
