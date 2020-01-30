module Main exposing(main)

import Clock
import Config exposing(Conf, ScreenStyle(..), screenValues)
import Parsing exposing(parseUrl)

import Browser
import Browser.Navigation as BN
import Css as C
import Css.Media as CM
import Html.Styled as HS
import Html.Styled.Attributes as HA
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Task
import Time
import Url

-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

-- MODEL

type alias Model =
    { key : BN.Key
    , url : Url.Url
    , zone : Time.Zone
    , time : Time.Posix
    , conf : Conf
    }

initModel : BN.Key -> Url.Url -> Model
initModel key url =
    Model
        key
        url
        Time.utc
        (Time.millisToPosix 0)
        (parseUrl url)

init : () -> Url.Url -> BN.Key -> (Model, Cmd Msg)
init _ url key = (initModel key url, Task.perform SetZone Time.here)

-- UPDATE

type Msg
    = SetZone Time.Zone
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetZone nzone -> ({model | zone = nzone}, Cmd.none)
        Tick ntime -> ({model | time = ntime}, Cmd.none)
        LinkClicked _ -> (model, Cmd.none)
        UrlChanged url -> ({model | url = url, conf = parseUrl url}, Cmd.none)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions _ = Time.every 1000 Tick

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        clockZone =
            viewClockZone
                model.conf.bgCol
                model.conf.screen
                (Clock.getClock model.conf model.time model.zone)
    in
    { title = "Vega"
    , body = [HS.toUnstyled clockZone]
    }

viewClockZone : String -> ScreenStyle -> List (S.Svg Msg) -> HS.Html Msg
viewClockZone bgCol screen content =
    let
        (sWidth, sHeight) = screenValues screen
        calcRatio = toFloat sWidth / toFloat sHeight
    in
    HS.div
        [ HA.css
            [ C.position C.fixed
            , C.top C.zero
            , C.bottom C.zero
            , C.right C.zero
            , C.left C.zero
            , C.backgroundColor (C.hex bgCol)
            ]
        ]
        [ HS.div
            [ HA.css
                [ C.position C.absolute
                , C.top C.zero
                , C.bottom C.zero
                , C.left C.zero
                , C.right C.zero
                , C.textAlign C.center
                , C.width (C.vh (100*calcRatio))
                , C.height (C.vh 100)
                , CM.withMedia
                    [CM.all [CM.maxAspectRatio (CM.ratio sWidth sHeight)]]
                    [C.width (C.vw 100), C.height (C.vw (100/calcRatio))]
                , C.margin C.auto
                , C.boxSizing C.borderBox
                , C.padding (C.px 10)
                ]
            ]
            [ S.svg
                [ SA.css
                    [ C.width (C.pct 100)
                    , C.height (C.pct 100)
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
