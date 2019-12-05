import Clock exposing(..)
import Config exposing(..)

import Browser
import Css exposing(..)
import Css.Media as CM
import Html.Styled exposing(..)
import Html.Styled.Attributes as HA
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Task
import Time

import Debug

-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , conf : Conf
    }

initModel : Model
initModel =
    Model
        Time.utc
        (Time.millisToPosix 0)
        getConfig

init : () -> (Model, Cmd Msg)
init _ = (initModel, Task.perform SetZone Time.here)

-- UPDATE

type Msg
    = SetZone Time.Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetZone nzone -> ({model | zone = nzone}, Cmd.none)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Vega - CUSTOMIZATION"
    , body =
        [ toUnstyled
            (div
                []
                [ viewDateSelector
                , viewLink
                , viewClockCustom model
                , viewClockSettings
                , div [HA.css [property "clear" "both"]] [text ""]
                ]
            )
            ]
    }

viewDateSelector : Html Msg
viewDateSelector =
    div
        [ HA.css
            [ margin (px 10)
            , border3 (px 2) solid (hex "#000")
            , padding (px 10)
            ]
        ]
        [
        label [] [text ":"]
        , input [] []
        , label [] [text ":"]
        , input
            []
            []
        ]

viewLink : Html Msg
viewLink =
    div
        []
        [text "custom link"]

viewClockCustom : Model -> Html Msg
viewClockCustom model =
    let
        (rWidth, rHeight) = Tuple.mapBoth toFloat toFloat (screenValues model.conf.screen)
        --margL = if
    in
    div
        [ HA.css
            [ margin (px 10)
            , marginLeft (px 10)
            , backgroundColor (hex model.conf.bgCol)
            , width (px (rWidth*200))
            , height (px (rHeight*200))
            , float left
            , CM.withMedia
                [CM.all [CM.maxWidth (px 700)]]
                [property "float" "none", marginRight auto, marginLeft auto]
            , CM.withMedia
                [CM.all [CM.maxWidth (px 950)]]
                [width (px (rWidth*100)), height (px (rHeight*100))]
            , CM.withMedia
                [CM.all [CM.maxWidth (px 1200)]]
                [width (px (rWidth*150)), height (px (rHeight*150))]
            ]
        ]
        [ S.svg
            [ SA.css
                [ width (pct 100)
                , height (pct 100)
                ]
            , SA.viewBox
                ("0 0 " ++ (ftos rWidth) ++ "00" ++ " " ++ (ftos rHeight) ++ "00")
            ]
            (getClock model.conf model.time model.zone)
        ]

viewClockSettings : Html Msg
viewClockSettings =
    div
        [ HA.css
            [ margin (px 10)
            , float right
            , width (calc (pct 100) minus (px 360))
            , border3 (px 2) solid (hex "#000")
            , boxSizing borderBox
            , CM.withMedia
                [CM.all [CM.maxWidth (px 700)]]
                [property "float" "none", width auto]
            ]
        ]
        [text "settings panel"]
