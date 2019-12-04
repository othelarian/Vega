import Config exposing(..)

import Browser
import Html.Styled exposing(..)

-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias Model = Int

init : () -> (Model, Cmd Msg)
init _ = (2, Cmd.none)

-- UPDATE

type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Vega - CUSTOMIZATION"
    , body = [toUnstyled (div [] [])]
    }

viewDateSelector : Html Msg
viewDateSelector =
    --
    --
    div [] [text "test 123"]
    --

viewClockCustom : Html Msg
viewClockCustom =
    --
    -- TODO : max 200x300 or 300x200
    --
    div [] []
    --

viewClockSettings : Html Msg
viewClockSettings =
    --
    --
    div [] []
    --
