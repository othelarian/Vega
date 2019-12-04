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

-- HELPERS

itos s = String.fromInt s
ftos s = String.fromFloat s

getCenterTransform : (Int, Int) -> S.Attribute Msg
getCenterTransform (centerX, centerY) =
    SA.transform ("translate("++(String.fromInt centerX)++" "++(String.fromInt centerY)++")")

buildArc : Int -> Int -> ConfArc -> Bool -> S.Svg Msg
buildArc val limit confArc par =
    let
        radS = itos confArc.radius
        pathBase = [ "m", "0,-"++radS, "A", radS, radS, "0"]
        arcStyle =
            [ SA.stroke confArc.col
            , SA.strokeWidth (itos confArc.thickness)
            , SA.fill "none"
            ]
        dVal =
            if val == 0 && (not par) then
                ["0 1 0,"++radS, "A", radS, radS, "0 0 1 0,-"++radS]
            else if val == 0 && par then
                ["0 1 0,-"++radS]
            else
                let
                    (fx, fy) =
                        let
                            dg = degrees (toFloat (val*confArc.coeff + -90))
                            rayf = toFloat confArc.radius
                        in
                        ((cos dg)*rayf, (sin dg)*rayf)
                    (large, swap) =
                        if par then if val < (limit//2 +1) then ("0", "1") else ("1", "1")
                        else if val < (limit//2) then ("1", "0") else ("0", "0")
                in
                [large, swap, (ftos fx), (ftos fy)]
        fdVal = String.join " " (List.append pathBase dVal)
    in
    S.path ((SA.d fdVal)::arcStyle) []

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        clockZone =
            viewClockZone
                model.conf.bgCol
                model.conf.ratioWidth
                model.conf.ratioHeight
                (viewSvgContent model)
    in
    { title = "Vega"
    , body = [toUnstyled (clockZone)]
    }

viewClockZone : String -> Int -> Int -> List (S.Svg Msg) -> Html Msg
viewClockZone bgCol rWidth rHeight content =
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

viewSvgContent : Model -> List (S.Svg Msg)
viewSvgContent model =
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
        , getSvg4Hours center model.conf.hours hrs (hrs//12 == 0)
        , getSvg4Minutes center model.conf.minutes mins (modBy 2 hrs == 0)
        , getSvg4Seconds center model.conf.seconds secs (modBy 2 mins == 0)
        ]
    --

getSvg4Seconds : (Int, Int) -> ConfSecond -> Int -> Bool -> List (S.Svg Msg)
getSvg4Seconds center config second minMod =
    let
        --
        --
        circ = buildArc second 60 config.confCirc minMod
        --
        --
    in
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
    let
        --
        --
        circ = buildArc minute 60 config.confCirc hrMod
        --
    in
    [ S.g
        [getCenterTransform center]
        [
        --
            circ
        --
        --
        ]
    ]

getSvg4Hours : (Int, Int) -> ConfHour -> Int -> Bool -> List (S.Svg Msg)
getSvg4Hours center config hour hrHalf =
    let
        --
        circ = buildArc (modBy 12 hour) 12 config.confCirc hrHalf
        --
    in
    --
    --
    [ S.g
        [getCenterTransform center]
        [
        --
            circ
        --
        --
        ]
    ]

-- TODO : Day of the week
-- TODO : Day of the month
-- TODO : Week of the year
-- TODO : Month
