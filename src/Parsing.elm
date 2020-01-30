module Parsing exposing(parseUrl, buildInfos, buildUrl)

import Config as Cfg

import Url

--import Debug

parseUrl : Url.Url -> Cfg.Conf
parseUrl url =
    let
        base = Cfg.getConfig
        fconf =
            case String.split "?" (Url.toString url) of
                _::infos::[] ->
                    case String.split ":" infos of
                        frst::_::_::[] ->
                            let
                                defscreen =
                                    case frst of
                                        "0" -> Cfg.Landscape
                                        "1" -> Cfg.Portrait
                                        _ -> base.screen
                                --
                                --
                                --
                            in
                            --
                            {base | screen = defscreen}
                            --
                        _ -> base
                _ -> base
    in
    fconf

buildUrl : Url.Url -> Cfg.Conf -> String
buildUrl _ _ =
    let
        --
        --surl = Url.toString url
        --
        --frstpart = ""
        --
        _ = ""
        --
    in
    --
    --
    ""
    --

buildInfos : Cfg.Conf -> String
buildInfos _ =
    --
    let
        --
        --
        _ = ""
        --
    in
    --
    ""
    --

-- first segment : SCREEN
-- 1 char, 0 or 1, for Landscape or Portrait
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

-- NEEDLE
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
