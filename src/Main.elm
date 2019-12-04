import Clock exposing(..)
import Config exposing(Conf, ScreenStyle(..), getConfig, screenValues)

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
    | Tick Time.Posix
    --
    | Tst Time.Posix
    --

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetZone nzone -> ({model | zone = nzone}, Cmd.none)
        Tick ntime -> ({model | time = ntime}, Cmd.none)
        --
        Tst _ ->
            let ntime = Time.millisToPosix ((Time.posixToMillis model.time) + 360000) in
            ({model | time = ntime}, Cmd.none)
        --

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions _ = Time.every 1000 Tick
--subscriptions _ = Time.every 200 Tst

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        clockZone =
            viewClockZone
                model.conf.bgCol
                model.conf.screen
                (getClock model.conf model.time model.zone)
    in
    { title = "Vega"
    , body = [toUnstyled (clockZone)]
    }

viewClockZone : String -> ScreenStyle -> List (S.Svg Msg) -> Html Msg
viewClockZone bgCol screen content =
    let
        (sWidth, sHeight) = screenValues screen
        calcRatio = toFloat sWidth / toFloat sHeight
    in
    div
        [ HA.css
            [ position fixed
            , top zero
            , bottom zero
            , right zero
            , left zero
            , backgroundColor (hex bgCol)
            ]
        ]
        [ div
            [ HA.css
                [ position absolute
                , top zero
                , bottom zero
                , left zero
                , right zero
                , textAlign center
                , width (vh (100*calcRatio))
                , height (vh 100)
                , CM.withMedia
                    [CM.all [CM.maxAspectRatio (CM.ratio sWidth sHeight)]]
                    [width (vw 100), height (vw (100/calcRatio))]
                , margin auto
                , boxSizing borderBox
                , padding (px 10)
                ]
            ]
            [ S.svg
                [ SA.css
                    [ width (pct 100)
                    , height (pct 100)
                    ]
                , SA.viewBox
                    ( "0 0 "
                    ++ String.fromInt (sWidth*100)
                    ++ " "
                    ++ String.fromInt (sHeight*100)
                    )
                ]
                content
            ]
        ]
