module Config exposing(..)

getConfig : Conf
getConfig =
    Conf
        3 -- ratioWidth
        2 -- ratioHeight
        "#000" -- bgCol
        (ConfSecond
            (ConfShow True True True)
            --
            Both -- needleStyle
            "#0f0" -- needleCol
            --
            (ConfArc 55 "#faf" 3 6)
            --
            --
        )
        (ConfMinute
            (ConfShow True True True)
            --
            (ConfArc 60 "orange" 3 6)
            --
        )
        (ConfHour
            (ConfShow True True True)
            --
            (ConfArc 65 "red" 3 30)
            --
        )

type alias Conf =
    { ratioWidth : Int
    , ratioHeight : Int
    , bgCol : String
    , seconds : ConfSecond
    , minutes : ConfMinute
    , hours : ConfHour
    }

type NeedleStyle
    = Front
    | Back
    | Both

type alias ConfShow =
    { needle : Bool
    , circle : Bool
    , tick : Bool
    }

type alias ConfArc =
    { radius : Int
    , col : String
    , thickness : Int
    , coeff : Int
    }

type alias ConfSecond =
    { show : ConfShow
    --
    , needleStyle : NeedleStyle
    , needleCol : String
    --
    , confCirc : ConfArc
    --
    -- TODO : col of circle
    --
    -- TODO : tick
    --
    }

type alias ConfMinute =
    { show : ConfShow
    --
    , confCirc : ConfArc
    --
    }

type alias ConfHour =
    { show : ConfShow
    --
    , confCirc : ConfArc
    --
    }
