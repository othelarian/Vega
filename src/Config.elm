module Config exposing
    ( Conf
    , ConfArc
    , ConfPart
    , DeviationStyle(..)
    , NeedleStyle(..)
    , ScreenStyle(..)
    , getConfig
    , screenValues
    )

getConfig : Conf
getConfig =
    Conf
        Landscape -- screen
        "#000000ff" -- bgCol
        (ConfPart -- seconds
            Nothing -- needle (TO CHANGE)
            (Just (ConfArc 55 "#ffaaffff" 3 6)) -- arcFront (TO CHANGE)
            Nothing -- arcBack (TO CHANGE)
            Nothing -- tickFront (TO CHANGE)
            Nothing -- tickFront5 (TO CHANGE)
            Nothing -- tickBack (TO CHANGE)
            Nothing -- tickBack5 (TO CHANGE)
            Nothing -- deviated
        )
        (ConfPart -- minutes
            Nothing -- needle (TO CHANGE)
            (Just (ConfArc 60 "#ffcc88ff" 3 6)) -- arcFront (TO CHANGE)
            Nothing -- arcBack (TO CHANGE)
            Nothing -- tickFront (TO CHANGE)
            Nothing -- tickFront5 (TO CHANGE)
            Nothing -- tickBack (TO CHANGE)
            Nothing -- tickBack5 (TO CHANGE)
            Nothing -- deviated
        )
        (ConfPart -- hours
            Nothing -- needle (TO CHANGE)
            (Just (ConfArc 65 "#ff0000ff" 3 30)) -- arcFront (TO CHANGE)
            Nothing -- arcBack (TO CHANGE)
            Nothing -- tickFront (TO CHANGE)
            Nothing -- tickFront5 (TO CHANGE)
            Nothing -- tickBack (TO CHANGE)
            Nothing -- tickBack5 (TO CHANGE)
            Nothing -- deviated
        )
        --
        --

{-
        (ConfHour
            (ConfShow True True True)
            --
            (ConfArc 65 "#ff0000ff" 3 30)
-}


screenValues : ScreenStyle -> (Int, Int)
screenValues screen =
    case screen of
        Landscape -> (3, 2)
        Portrait -> (2, 3)

type alias Conf =
    { screen : ScreenStyle
    , bgCol : String
    , seconds : ConfPart
    , minutes : ConfPart
    , hours : ConfPart
    -- day in week
    -- day in month
    -- month
    }

-- TYPES

type ScreenStyle
    = Landscape
    | Portrait

type NeedleStyle
    = StyleClassic

type DeviationStyle
    = DeviationNone
    | DeviationSecond
    | DeviationMinute

-- BUILDER CONF

type alias ConfNeedle =
    { style : NeedleStyle
    , start : Int -- starting point, eq "needle distance"
    , length : Int
    }

type alias ConfArc =
    { radius : Int
    , color : String
    , thickness : Int
    , coeff : Int
    }

type alias ConfTick =
    { radius : Int
    , color : String
    , thickness : Int
    , length : Int
    }

type alias ConfDeviation =
    { style : DeviationStyle
    , distance : Int
    }

type alias ConfPart =
    { needle : Maybe ConfNeedle
    , arcFront : Maybe ConfArc
    , arcBack : Maybe ConfArc
    , tickFront : Maybe ConfTick
    , tickFront5 : Maybe ConfTick
    , tickBack : Maybe ConfTick
    , tickBack5 : Maybe ConfTick
    , deviated : Maybe ConfDeviation
    }
