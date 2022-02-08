module Caesar exposing (..)
import Html exposing (Html)

cypher : Char -> Int -> Char
cypher letter offset = 
    if (Char.toCode letter) > 64 && (Char.toCode letter) < 123 then
        Char.fromCode((Char.toCode letter) + offset)
    else
        letter


input_list = String.toList "ABCD CAESAR"
cypher_list = List.map (\x -> cypher x 5) input_list

final_string = String.fromList cypher_list

results: List String
results = 
    [
        "-- Hello-Elm output --\n\n  Cypher calculations:",
        final_string,
        "\n-- end --"
    ]

page_width = 80

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div (results))