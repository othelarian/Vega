module Clock exposing(getClock)

import Config exposing(..)

import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Time

import Debug

getClock : Conf -> Time.Posix -> Time.Zone -> List (S.Svg msg)
getClock conf time zone =
    let
        (rWidth, rHeight) = screenValues conf.screen
        center = (rWidth*50, rHeight*50)
        --
        secs = (Time.toSecond zone time)
        mins = (Time.toMinute zone time)
        hrs = (Time.toHour zone time)
        --
    in
    
    --
    List.concat
        [ []
        --
        --
        , componentHours center conf.hours hrs (hrs//12 == 0)
        , componentMinutes center conf.minutes mins (modBy 2 hrs == 0)
        , componentSeconds center conf.seconds secs (modBy 2 mins == 0)
        ]
    --

-- HELPERS

itos s = String.fromInt s
ftos s = String.fromFloat s

getCenterTransform : (Int, Int) -> S.Attribute msg
getCenterTransform (centerX, centerY) =
    SA.transform ("translate("++(String.fromInt centerX)++" "++(String.fromInt centerY)++")")

-- COMPONENTS

componentSeconds : (Int, Int) -> ConfSecond -> Int -> Bool -> List (S.Svg msg)
componentSeconds center config second minMod =
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

componentMinutes : (Int, Int) -> ConfMinute -> Int -> Bool -> List (S.Svg msg)
componentMinutes center config minute hrMod =
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

componentHours : (Int, Int) -> ConfHour -> Int -> Bool -> List (S.Svg msg)
componentHours center config hour hrHalf =
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

-- BUILDERS

buildArc : Int -> Int -> ConfArc -> Bool -> S.Svg msg
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
