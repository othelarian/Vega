module Clock exposing(getClock, itos, ftos)

import Config as Cfg

import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Time

--import Debug


getClock : Cfg.Conf -> Time.Posix -> Time.Zone -> List (S.Svg msg)
getClock conf time zone =
    let
        (rWidth, rHeight) = Cfg.screenValues conf.screen
        center = (rWidth*50, rHeight*50)
        --
        secs = Time.toSecond zone time
        mins = Time.toMinute zone time
        hrs = Time.toHour zone time
        --
        testScreen = case conf.screen of
            Cfg.Landscape -> S.rect [SA.x "2", SA.y "2", SA.width "20", SA.height "10", SA.fill "red"] []
            Cfg.Portrait -> S.rect [SA.x "2", SA.y "2", SA.width "10", SA.height "20", SA.fill "green"] []
        --
    in
    --
    List.concat
        [ [testScreen]
        --
        --
        --, componentHours center conf.hours hrs (hrs//12 == 0)
        --, componentMinutes center conf.minutes mins (modBy 2 hrs == 0)
        , component center conf.seconds secs (modBy 2 mins == 0)
        ]
    --

-- HELPERS

itos : Int -> String
itos s = String.fromInt s
ftos : Float -> String
ftos s = String.fromFloat s

getCenterTransform : (Int, Int) -> S.Attribute msg
getCenterTransform (centerX, centerY) =
    SA.transform ("translate("++String.fromInt centerX++" "++String.fromInt centerY++")")

-- COMPONENTS

component : (Int, Int) -> Cfg.ConfPart -> Int -> Bool -> List (S.Svg msg)
component center config value valMod =
    let
        --
        --
        circ = buildArc value 60 config.confCirc valMod
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

-- BUILDERS

buildNeedle : S.Svg msg
buildNeedle =
    --
    --
    --
    S.line [] []

buildArc : Int -> Int -> Cfg.ConfArc -> Bool -> S.Svg msg
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
            if val == 0 && not par then
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
                        ( cos dg *rayf, sin dg *rayf)
                    (large, swap) =
                        if par then if val < (limit//2 +1) then ("0", "1") else ("1", "1")
                        else if val < (limit//2) then ("1", "0") else ("0", "0")
                in
                [large, swap, ftos fx, ftos fy]
        fdVal = String.join " " (List.append pathBase dVal)
    in
    S.path ( SA.d fdVal ::arcStyle) []
