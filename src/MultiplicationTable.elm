module MultiplicationTable exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Browser
import Browser.Navigation as Nav
import Json.Decode as Decode
import Url
import Element
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font



-- PRIMARY DECLARATION


main : Program Decode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = (\_ -> Noop) --UrlChanged
    , onUrlRequest = (\_ -> Noop) --LinkClicked
    }



-- MODEL


type Model =
  Model Int



-- INIT


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
  ( Model 0, Cmd.none )


-- UPDATE


type Msg
  = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model modelRecord) as model) =
  let
    x=Debug.log "update msg=" msg
    
  in
    case msg of
      Noop ->
        ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


body : Model -> List ( Html Msg )
body ( (Model modelRecord) as model) =
  [ Element.layout
    []
    <|Element.text "Hello World"
  ]

view : Model -> Browser.Document Msg
view model =
  { title = "Multiplication Tables"
  , body = body model
  }
