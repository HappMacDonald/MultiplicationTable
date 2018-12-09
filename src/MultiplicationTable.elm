module MultiplicationTable exposing (main)

import Browser
import Browser.Navigation as Nav
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Url
import Array exposing (Array)



-- PRIMARY DECLARATION


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Noop --UrlChanged
        , onUrlRequest = \_ -> Noop --LinkClicked
        }



-- MODEL


type Model
    = Model ( Array ( Array ( Maybe Int ) ) )



-- INIT


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( Model
        ( Array.repeat 10 Nothing
          |>Array.repeat 10
        ), Cmd.none )



-- UPDATE


type Msg
    = OnEntry Int Int String
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model modelRows) as model) =
    let
        x =
            Debug.log "update msg=" msg
    in
    case msg of
        OnEntry rowNum colNum cellStr ->
            let
                rowOld =
                    modelRows
                    |>Array.get rowNum
                    |>Maybe.withDefault ( Array.repeat 10 Nothing )

                cellOld =
                    rowOld
                    |>Array.get colNum
                    |>Maybe.withDefault Nothing

                cellNew =
                    case String.trim cellStr of
                        "" ->
                            Nothing

                        _ ->
                            String.toInt cellStr
                            |>Maybe.map
                                (\cellNum ->
                                if cellNum>=0 && cellNum<100 then
                                    Just cellNum
                                else
                                    cellOld
                                )
                            |>Maybe.withDefault cellOld
            in
                ( Model
                    ( modelRows
                    |>Array.set
                        rowNum
                        ( Array.set
                            colNum
                            cellNew
                            rowOld
                        )
                    )
                , Cmd.none
                )

        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


cellColor : Element.Color
cellColor =
    Element.rgb 0.6 0.5 0.4


tableBorderColor : Element.Color
tableBorderColor =
    Element.rgb 0.2 0.2 0.2


tableBorder : Int
tableBorder =
    3


inputWidth : Int
inputWidth=
    56


centerWrap : Element.Element Msg -> Element.Element Msg
centerWrap inside =
    inside
    |>Element.el
        [ Element.centerX
        , Element.centerY
        ]


body : Model -> List (Html Msg)
body ((Model modelRows) as model) =
    [ modelRows
    |>Array.indexedMap
        (\rowNum row ->
            case rowNum of
                0 ->
                    Element.none

                1 ->
                    List.range 1 9
                    |>List.map
                        (\colNum ->
                        Element.text ( String.fromInt colNum )
                        |>centerWrap
                        |>Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Background.color cellColor
                            ]
                        )
                    |>Element.row -- collection of top headers
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing tableBorder
                        ]

                _ -> -- 2-9
                    ( row
                    |> Array.indexedMap
                        (\colNum cell ->
                        case colNum of
                            0 ->
                                Element.none

                            1 ->
                                Element.text ( String.fromInt rowNum )
                                |>centerWrap
                                |>Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Background.color cellColor
                                    ]
                            
                            _ -> -- 2-9
                                Input.text
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Element.centerX
                                    ]
                                    { onChange = OnEntry rowNum colNum
                                    , text =
                                        cell
                                        |>Maybe.map
                                            String.fromInt
                                        |>Maybe.withDefault ""
                                    , placeholder = Nothing
                                    , label = Input.labelLeft [] Element.none
                                    }
                                |>Element.el
                                    [ Element.fill
                                      |>Element.maximum inputWidth
                                      |>Element.width 
                                    , Element.height Element.fill
                                    , Background.color cellColor
                                    ]
                        )
                    |>Array.toList
                    |>Element.row -- collection of cells
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacing tableBorder
                        ]
                    )

        )
    |>Array.toList
    |>Element.column -- collection of rows
        [ Element.spacing tableBorder
        , Element.padding tableBorder
        ]
    |>Element.el -- container
        [ Element.centerX
        , Element.centerY
        , Background.color tableBorderColor
        ]        
    |>Element.layout -- page
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Multiplication Tables"
    , body = body model
    }
