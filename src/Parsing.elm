module Parsing exposing(parseUrl, buildInfos, buildUrl)

import Config as Cfg

import Url

--import Debug

parseUrl : Url.Url -> Cfg.Conf
parseUrl url =
    let
        base = Cfg.getConfig
        tconf query =
            case String.split ":" query of
                frst::_::_::[] ->
                    let
                        -- SCREEN ===================================
                        (defscreen, bgcol) =
                            let len = String.length frst in
                            if String.length frst > 0 then
                                if len == 1 then
                                    if frst == "1" then (Cfg.Portrait, base.bgCol)
                                    else (Cfg.Landscape, base.bgCol)
                                else if len == 8 then (base.screen, "#"++frst)
                                else
                                    ( if String.left 1 frst == "1" then Cfg.Portrait else Cfg.Landscape
                                    , "#"++ String.dropLeft 1 frst
                                    )
                            else (base.screen, base.bgCol)
                        -- VARIATIONS ===============================
                        --
                        --
                        --
                        -- SETTINGS =================================
                        --
                        --
                    in
                    Cfg.Conf
                        defscreen
                        bgcol
                        (Cfg.ConfSecond
                            (Cfg.ConfShow True True True)
                            --
                            Cfg.StyleBoth -- needleStyle
                            "#00ff00ff" -- needleCol
                            --
                            (Cfg.ConfArc 55 "#ffaaffff" 3 6)
                            --
                            --
                        )
                        (Cfg.ConfMinute
                            (Cfg.ConfShow True True True)
                            --
                            (Cfg.ConfArc 60 "#ffcc88ff" 3 6)
                            --
                        )
                        (Cfg.ConfHour
                            (Cfg.ConfShow True True True)
                            --
                            (Cfg.ConfArc 65 "#ff0000ff" 3 30)
                            --
                        )
                _ -> base
        fconf =
            case url.query of
                Just query -> tconf query
                Nothing -> base
    in
    fconf

buildUrl : Url.Url -> Cfg.Conf -> Url.Url
buildUrl url conf =
    {url | query = Just (buildInfos conf)}

buildInfos : Cfg.Conf -> String
buildInfos conf =
    let
        -- SCREEN =========================================
        frst =
            let
                orientation = if conf.screen == Cfg.Portrait then "1" else ""
                color = if conf.bgCol /= "#000000ff" then String.dropLeft 1 conf.bgCol else ""
            in
            orientation ++ color
        -- VARIATIONS =====================================
        --
        snd = ""
        --
        -- SETTINGS =======================================
        --
        thrd = ""
        --
        --
        -- FINAL ==========================================
        fnl = String.join ":" [frst, snd, thrd]
    in
    if fnl == "::" then "" else fnl

-- first segment : SCREEN
-- 1 char, 0 or 1, for Landscape or Portrait, + 8 chars, color of the background => 9 chars
-- second segment : VARIATIONS
-- total : 2 symbols, for 8 bits, for each elements => 12 symbols, base32 format
-- third segment : SETTINGS
-- subtotal : 1 needle (20), 2 arcs (15*2 = 30), 2 ticks (18*2 = 36) => 86 chars
-- total : 86*12 + 11 (separators) = 1043 chars

-- all segments (+ separators) => 1058 chars

-- ELEMENTS

-- seconds
-- minutes
-- hours
-- day of the week
-- day of the month
-- month

-- VARIATIONS

-- NEEDLE ??? (I forget everything about this stuff)
-- no needle
-- needle classic
-- needle double, only bottom
-- needle double, only top
-- needle double, both
-- TOTAL : 5 -> 8 =========> 3 bits
-- ARC
-- no arc
-- front arc active
-- back arc active
-- both arc active
-- TOTAL : 4 -> 2 =========> 2 bits
-- TICK
-- no tick
-- front tick active
-- back tick active
-- both tick active
-- TOTAL : 4 -> 2 =========> 2 bits
-- SPECIAL
-- nothing
-- element deviated (seconds and minutes)
-- TOTAL : 2 -> 1 =========> 1 bit

-- TOTAL VARIATIONS = 8 bits

-- SETTINGS

-- needle distance : SInt, 3 digits (+1 sign, but limited if reversed)
-- needle length : UInt, 3 digits
-- needle separation : SInt, 3 digits (+1 sign, but limited if reversed)
-- needle color : max 8 chars
-- TOTAL : 17 + 3 = 20 chars

-- arc distance : UInt, 3 digits
-- arc thickness : UInt, 2 digits (divided by 10)
-- arc color : max 8 chars
-- TOTAL : 13 + 2 = 15 chars

-- tick distance : UInt, 3 digits
-- tick length : UInt, 2 digits
-- tick thickness : UInt, 2 digits (divided by 10)
-- tick color : max 8 chars
-- TOTAL : 15 + 3 = 18 chars
