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

members: Proposition -> List String
members input = 
    case input of
        Or prop prop1 -> List.append (members prop) (members prop1)
        Not prop-> members prop
        And prop prop1 ->  List.append (members prop) (members prop1)
        Impl prop prop1 ->  List.append (members prop) (members prop1)
        Bi prop prop1 ->  List.append (members prop) (members prop1)
        Var string ->  [string]

parseDemorgan: Proposition -> String
parseDemorgan input = 
    case input of
        Or prop prop1 -> "(" ++ (parseDemorgan prop) ++ " or " ++ (parseDemorgan prop1) ++ ")"
        Not prop-> if (String.contains "and" (parse prop)) || (String.contains "or" (parse prop)) then (demorgan prop) else "(" ++ "! " ++ (parseDemorgan prop) ++ ")"
        And prop prop1 ->  "(" ++ (parseDemorgan prop) ++ " and " ++  (parseDemorgan prop1) ++ ")"
        Impl prop prop1 ->  "(" ++ "!" ++ (parseDemorgan prop) ++ " or " ++  (parseDemorgan prop1) ++ ")"
        Bi prop prop1 ->  "(" ++ (parseDemorgan prop) ++ " or " ++  "not " ++ (parseDemorgan prop1) ++ ")"
        Var string ->  string

demorgan: Proposition -> String
demorgan inputProp = 
     case inputProp of
                    Or prop prop1 -> "(" ++ (parse (Not prop)) ++ " or " ++ (parse (Not prop1)) ++ ")"
                    Not prop -> "(" ++ "! " ++ (parse prop) ++ ")"
                    And prop prop1 ->  "(" ++ (parse (Not prop)) ++ " and " ++  (parse (Not prop1)) ++ ")"
                    Impl prop prop1 ->  "(" ++ (parse prop) ++ " => " ++  (parse prop1) ++ ")"
                    Bi prop prop1 ->  "(" ++ (parse prop) ++ " <=> " ++  (parse prop1) ++ ")"
                    Var string ->  string


unique : List a -> List a
unique l = 
    let
        incUnique : a -> List a -> List a
        incUnique elem lst = 
            case List.member elem lst of
                True -> lst
                False -> elem :: lst
    in
        List.foldr incUnique [] l

results : List String
results =
    [ "-- Hello-Elm output --\n\n",
      parse (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
      parse (Impl (Var "B") (Var "C")),
      parse (Or (Impl (Var "B") (Not (Var "C"))) (Bi (Not (Var "D")) (Var "E"))),
      "\n",
      "-- SWITCHEROO --\n",
      switcheroo (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
      switcheroo (Impl (Var "B") (Var "C")),
      switcheroo (Or (Impl (Var "B") (Not (Var "C"))) (Bi (Not (Var "D")) (Var "E"))),
      "-- MEMBERS --",
      Debug.toString (unique (members (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "A"))))))),
      "-- DeMorgan -- ",
      parseDemorgan (Not (And (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
      parseDemorgan (Not (Or (Var "A") (Or (Impl (Var "B") (Var "C")) (Bi (Var "D") (Var "E"))))),
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
