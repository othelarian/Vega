module Config exposing
    ( Conf
    , ConfArc
    , ConfHour
    , ConfMinute
    , ConfSecond
    , ConfShow
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
        (ConfSecond
            (ConfShow True True True)
            --
            StyleBoth -- needleStyle
            "#00ff00ff" -- needleCol
            --
            (ConfArc 55 "#ffaaffff" 3 6)
            --
            --
        )
        (ConfMinute
            (ConfShow True True True)
            --
            (ConfArc 60 "#ffcc88ff" 3 6)
            --
        )
        (ConfHour
            (ConfShow True True True)
            --
            (ConfArc 65 "#ff0000ff" 3 30)
            --
        )

screenValues : ScreenStyle -> (Int, Int)
screenValues screen =
    case screen of
        Landscape -> (3, 2)
        Portrait -> (2, 3)

type alias Conf =
    { screen : ScreenStyle
    , bgCol : String
    , seconds : ConfSecond
    , minutes : ConfMinute
    , hours : ConfHour
    }

-- TYPES

type ScreenStyle
    = Landscape
    | Portrait

type NeedleStyle
    --= Front
    --| Back
    --| Both
    = StyleBoth

-- BUILDER CONF

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

-- COMPONENTS CONF

type alias ConfSecond =
    { show : ConfShow
    --
    , needleStyle : NeedleStyle
    , needleCol : String
    --
    , confCirc : ConfArc
    --
    -- col of circle
    --
    -- tick
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
