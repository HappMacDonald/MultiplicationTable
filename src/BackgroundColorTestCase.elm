-- module BackgroundColorTestCase exposing (main)

import Browser
import Element
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    Bool


init : Model
init =
    True


type Msg
    = OnPress


update : Msg -> Model -> Model
update msg model =
    not model


view : Model -> Html Msg
view model =
    let
        indicatorColor =
            (if model then
                Element.rgb 1 0 0
                -- "red"

             else
                Element.rgb 0 1 0
                -- "green"
            )
                |> Debug.log "indicatorColor"
    in
    Input.button
        []
        { onPress = Just OnPress
        , label =
            (if model then
                "Red"

             else
                "Green"
            )
                |> Element.text
        }
        |> Element.layout
            [ 
                Background.color indicatorColor
                -- Html.Attributes.style "background-color" indicatorColor
                -- |>Element.htmlAttribute
            ]
