module Config exposing(..)

getConfig : Conf
getConfig =
    Conf
        3 -- ratioWidth
        2 -- ratioHeight
        "#000" -- bgCol
        (ConfSecond
            True -- showNeedle
            Both -- styleNeedle
            "#0f0" -- needleCol
            True -- showCirc
            --
            --
            True -- showTick
            --
        )
        (ConfMinute
            True -- showNeedle
        )

type alias Conf =
    { ratioWidth : Int
    , ratioHeight : Int
    , bgCol : String
    , seconds : ConfSecond
    , minutes : ConfMinute
    }

type NeedleStyle
    = Front
    | Back
    | Both

type alias ConfSecond =
    { showNeedle : Bool
    , styleNeedle : NeedleStyle
    , needleCol : String
    , showCirc : Bool
    --
    -- TODO : col of circle
    --
    -- TODO : tick
    --
    , showTick : Bool
    --
    }

type alias ConfMinute =
    { showNeedle : Bool
    }
