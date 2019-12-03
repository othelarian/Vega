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
    | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetZone nzone -> ({model | zone = nzone}, Cmd.none)
        Tick ntime -> ({model | time = ntime}, Cmd.none)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model = Time.every 1000 Tick

-- HELPERS

itos s = String.fromInt s
ftos s = String.fromFloat s

getCenterTransform : (Int, Int) -> S.Attribute Msg
getCenterTransform (centerX, centerY) =
    SA.transform ("translate("++(String.fromInt centerX)++" "++(String.fromInt centerY)++")")

buildArc : Int -> Int -> Int -> Bool -> String
buildArc ray val coeff par =
    let
        --
        --
        --
        --
        (fx, fy) =
            let
                dg = degrees (toFloat (val*coeff))
                rayf = toFloat ray
            in
            ((cos dg)*rayf, (sin dg)*rayf)
        --
        large =
            if par then if val < 31 then "0" else "1"
            else "0"
        swap = "0"
        --
    in
    --
    --
    String.join " " ["A", itos ray, itos ray, itos ray, large, swap, (ftos fx)++","++(ftos fy)]

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        clockZone =
            getClockZone
                model.conf.bgCol
                model.conf.ratioWidth
                model.conf.ratioHeight
                (getSvgContent model)
    in
    { title = "Vega"
    , body = [toUnstyled (clockZone)]
    }

getClockZone : String -> Int -> Int -> List (S.Svg Msg) -> Html Msg
getClockZone bgCol rWidth rHeight content =
    let calcRatio = toFloat rWidth / toFloat rHeight in
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
                    [CM.all [CM.maxAspectRatio (CM.ratio rWidth rHeight)]]
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
                , SA.viewBox ("0 0 " ++ String.fromInt (rWidth*100) ++ " " ++ String.fromInt (rHeight*100))
                ]
                content
            ]
        ]

getSvgContent : Model -> List (S.Svg Msg)
getSvgContent model =
    --
    let
        --
        center = (model.conf.ratioWidth*50, model.conf.ratioHeight*50)
        --
        secs = (Time.toSecond model.zone model.time)
        mins = (Time.toMinute model.zone model.time)
        hrs = (Time.toHour model.zone model.time)
        --
    in
    
    --
    List.concat
        [ []
        --
        --
        , getSvg4Minutes center model.conf.minutes mins (modBy 2 hrs == 0)
        , getSvg4Seconds center model.conf.seconds secs (modBy 2 mins == 0)
        ]
    --
    {-
    [ S.rect
        [ SA.x "10"
        , SA.y "10"
        , SA.width "10"
        , SA.height "10"
        , SA.fill "#0f0"
        ]
        []
    ]
    -}

getSvg4Seconds : (Int, Int) -> ConfSecond -> Int -> Bool -> List (S.Svg Msg)
getSvg4Seconds center config second minMod =
    --
    let
        --
        circStyle = [SA.stroke "#f00", SA.fill "none"]
        --
        circ =
            if second == 0 || second == 60 then
                --
                S.circle [] []
                --
            else
                --
                let circPath = "m 0,-60"++(buildArc 60 second 6 True) in
                --
                S.path ((SA.d circPath)::circStyle) []
                --
    in
    --
    --
    [ S.g
        [getCenterTransform center]
        [
        --
        --
            circ
        --
        --
        , S.g [SA.transform "rotate(0)"] [S.path [SA.d ""] []]
        ]
    ]

getSvg4Minutes : (Int, Int) -> ConfMinute -> Int -> Bool -> List (S.Svg Msg)
getSvg4Minutes center config minute hrMod =
    --
    --
    --
    []

getSvg4Hours : List (S.Svg Msg)
getSvg4Hours =
    --
    --
    --
    []

-- TODO : Day of the week
-- TODO : Day of the month
-- TODO : Week of the year
-- TODO : Month
