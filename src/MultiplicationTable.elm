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
import Ports exposing (youwin)



-- PRIMARY DECLARATION


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = body
        , update = update
        , subscriptions = subscriptions
        -- , onUrlChange = \_ -> Noop --UrlChanged
        -- , onUrlRequest = \_ -> Noop --LinkClicked
        }



-- MODEL


type Model =
    Model 
    { cells : ( Array ( Array ( Maybe Int ) ) )
    , wonyet : Bool
    }


-- INIT


{-|This constant defines the smallest integer greater than 2
which we will NOT be trying to multiply by.

For ordinary operation, you set this equal to 10 in order to support
multiplying up to 9x9.
-}
circumscription : Int
circumscription =
    10


{- Decode.Value -> Url.Url -> Nav.Key -}
init : a -> ( Model, Cmd Msg )
init {-flags url navKey-} _ =
    ( Model
      { cells =
          ( Array.repeat circumscription Nothing
          |>Array.repeat circumscription
          )
        
      , wonyet = False
      }
    , Cmd.none )



-- UPDATE


type Msg
    = OnEntry Int Int String
    | Reset
    | Noop


fail : String -> List Int -> String
fail message coords =
    message
    ++": "
    ++( coords
      |>List.map String.fromInt
      |>String.join ", "
      )



checkCells : Int -> Int -> List ( Maybe Int ) -> Bool
checkCells verticalFactor horizontalFactor cells =
    case cells of
        [] ->
            True

        thisCell :: remainingRows ->
            let
                thisValue =
                    Maybe.withDefault 0 thisCell

                correct =
                    horizontalFactor<2
                    ||verticalFactor<2
                    ||( thisValue == verticalFactor * horizontalFactor )

            in
            correct
            &&checkCells
                verticalFactor
                ( horizontalFactor + 1 )
                remainingRows



checkRows : Int -> List (Array ( Maybe Int )) -> Bool
checkRows verticalFactor rows =
    case rows of
        [] ->
            True

        thisRow :: remainingRows ->
            ( thisRow
            |>Array.toList
            |>checkCells verticalFactor 0
            )
            &&checkRows ( verticalFactor + 1 ) remainingRows


winningTable : Array ( Array ( Maybe Int ) ) -> Bool
winningTable table =
    table
    |>Array.toList
    |>checkRows 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model modelRecord) as model) =
    case msg of
        OnEntry rowNum colNum cellStr ->
            let
                rowOld =
                    modelRecord.cells
                    |>Array.get rowNum
                    |>Maybe.withDefault ( Array.repeat circumscription Nothing )

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
                
                newCells =
                    modelRecord.cells
                    |>Array.set
                        rowNum
                        ( Array.set
                            colNum
                            cellNew
                            rowOld
                        )

                wonyet = 
                    modelRecord.wonyet
                    ||winningTable newCells

                wonJustNow =
                    not modelRecord.wonyet
                    && wonyet
            in
                ( Model
                  { modelRecord
                  | cells = newCells
                  , wonyet = wonyet
                  }
                , if wonJustNow
                  then youwin ()
                  else Cmd.none
                )

        Reset ->
            init 0

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


-- incorrectColor : Element.Color
-- incorrectColor =
--     Element.rgb 1 0 0
    

-- correctColor : Element.Color
-- correctColor =
--     Element.rgb 0 1 0

buttonColor : Element.Color
buttonColor =
    Element.rgb 0 1 0


incorrectColor : String
incorrectColor =
    "red"
    

correctColor : String
correctColor =
    "green"


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


body : Model -> Html Msg
body ((Model modelRecord) as model) =
    [ modelRecord.cells
        |>Array.indexedMap
            (\rowNum row ->
                case rowNum of
                    0 ->
                        Element.none

                    1 ->
                        List.range 1 (circumscription-1)
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
                                    let
                                        cellStr =
                                            cell
                                            |>Maybe.map String.fromInt
                                            |>Maybe.withDefault ""

                                        cellNum =
                                            Maybe.withDefault 0 cell

                                        indicatorColor =
                                            ( if cellNum == rowNum * colNum
                                            then correctColor
                                            else incorrectColor
                                            )

                                    in
                                    Input.text
                                        [ Element.width Element.fill
                                        , Element.height Element.fill
                                        , Attr.style "background-color" indicatorColor
                                        |>Element.htmlAttribute

                                        , Element.centerX
                                        ]
                                        { onChange = OnEntry rowNum colNum
                                        , text = cellStr
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
        |>Element.column
            [ Background.color tableBorderColor
            , Element.spacing tableBorder
            , Element.padding tableBorder
            ]
      ]
    ++[ Element.text "\u{00A0}"
        , Input.button
        [ Element.width Element.fill
        , Element.padding 8
        , Font.center
        , Background.color buttonColor
        ]
        { onPress = Just Reset
        , label =
            Element.text "Start all over?"
        }
            
      ]
        
    |>Element.column -- vertical layout of ui
        [ Element.centerX
        , Element.centerY
        ]
    |>Element.el -- container
        [ Element.centerX
        , Element.centerY
        ]
    |>Element.layout -- page
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
