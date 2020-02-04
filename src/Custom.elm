module Custom exposing(main)

import Clock exposing(getClock, ftos)
import Config as Cfg
import Parsing exposing(buildInfos, buildUrl, parseUrl)

import Array
import Browser
import Browser.Navigation as BN
import Css as C
import Css.Media as CM
import Html.Styled as HS
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Maybe
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Task
import Time
import Url

-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

-- MODEL

type alias Color =
    { r : Int
    , g : Int
    , b : Int
    , a : Int
    }

type alias SettingsColor =
    { c1 : Color
    , c2 : Color
    , open : Bool
    }

initSettingsColor : String -> SettingsColor
initSettingsColor color =
    let hexcol = fromHex2Color color in
    SettingsColor hexcol hexcol False

type alias Settings =
    { bgCol : SettingsColor
    }

initSettings : Cfg.Conf -> Settings
initSettings conf =
    Settings
        (initSettingsColor conf.bgCol) -- bgCol

type alias Model =
    { key : BN.Key
    , url : Url.Url
    , zone : Time.Zone
    , time : Time.Posix
    , conf : Cfg.Conf
    , settings : Settings
    }

initModel : BN.Key -> Url.Url -> Model
initModel key url =
    let conf = parseUrl url in
    Model
        key
        url
        Time.utc
        (Time.millisToPosix 0)
        conf
        (initSettings conf)

init : () -> Url.Url -> BN.Key -> (Model, Cmd Msg)
init _ url key = (initModel key url, Task.perform SetZone Time.here)

-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    -- time messages
    | SetZone Time.Zone
    | SetCurrentTime Time.Posix
    | SelectTime String String
    | MoveTime Bool String
    -- screen messages
    | SetOrientation String
    -- color messages
    | ToggleColorSliders String
    | MoveColorSlider String String
    | SelectColorSlider String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked _ -> (model, Cmd.none)
        UrlChanged url -> ({model | url = url, conf = parseUrl url}, Cmd.none)
        -- time update
        SetZone nzone -> ({model | zone = nzone}, Task.perform SetCurrentTime Time.now)
        SetCurrentTime ntime -> ({model | time = ntime}, Cmd.none)
        -- screen update
        SetOrientation nscreen ->
            let
                ascreen = case nscreen of
                    "landscape" -> Cfg.Landscape
                    "portrait" -> Cfg.Portrait
                    _ -> model.conf.screen
                oconf = model.conf
                url = buildUrl model.url {oconf | screen = ascreen}
            in
            (model, BN.pushUrl model.key (Url.toString url))
        SelectTime ref value ->
            let
                v = String.toInt value |> Maybe.withDefault 0
                rng r c = case compare v r of
                    LT -> (r - v) * -1 * c
                    GT -> (v - r) * c
                    EQ -> 0
                otime = model.time
                trans (r, c) = Time.posixToMillis otime
                    |> (+) (rng r c)
                    |> Time.millisToPosix
                ntime = trans <| case ref of
                    "second" -> (Time.toSecond model.zone otime, 1000)
                    "minute" -> (Time.toMinute model.zone otime, 60000)
                    "hour" -> (Time.toHour model.zone otime, 3600000)
                    _ -> (0, 0)
            in
            ({model | time = ntime}, Cmd.none)
        MoveTime way ref ->
            let
                --
                --
                otime = model.time
                --
                --
                ntime = otime
                --
            in
            ({model | time = ntime}, Cmd.none)
        -- Color update
        ToggleColorSliders ref ->
            let
                togcol sc = {sc | c2 = sc.c1, open = not sc.open}
                (nsettings, nconf) = moveColor
                    model.settings
                    model.conf
                    ref
                    (\col -> togcol col)
                    (\col -> fromColor2Hex col.c1)
                    (\col -> col.open)
            in
            ({model | settings = nsettings, conf = nconf}, Cmd.none)
        MoveColorSlider ref value ->
            let
                prepMod sc =
                    let
                        modCol e c v = case e of
                            "r" -> {c | r = v}
                            "g" -> {c | g = v}
                            "b" -> {c | b = v}
                            "a" -> {c | a = v}
                            _ -> c
                    in
                    {sc | c2 = modCol (String.right 1 ref) sc.c2 (String.toInt value |> Maybe.withDefault 0)}
                sref = String.split "-" ref |> List.head |> Maybe.withDefault ""
                (nsettings, nconf) = moveColor
                    model.settings
                    model.conf
                    sref
                    (\col -> prepMod col)
                    (\col -> fromColor2Hex col.c2)
                    (\_ -> True)
            in
            ({model | settings = nsettings, conf = nconf}, Cmd.none)
        SelectColorSlider ref ->
            let
                (nsettings, nconf) = moveColor
                    model.settings
                    model.conf
                    ref
                    (\col -> {col | c1 = col.c2, open = False})
                    (\col -> fromColor2Hex col.c2)
                    (\_ -> True)
                url = buildUrl model.url nconf
            in
            ({model | settings = nsettings}, BN.pushUrl model.key (Url.toString url))
        --
        --
        --
        --

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- HELPERS

fromHex2Color : String -> Color
fromHex2Color hex =
    let
        src = "0123456789abcdef"
        toHexInt v = String.indexes v src |> List.head |> Maybe.withDefault 1
        toColInt v = toHexInt (String.left 1 v) * 16 + toHexInt (String.right 1 v)
        h = String.dropLeft 1 hex
        r = String.left 2 h
        g = String.left 4 h |> String.dropLeft 2
        b = String.right 4 h |> String.dropRight 2
        a = String.right 2 h
    in
    Color (toColInt r) (toColInt g) (toColInt b) (toColInt a)

fromColor2Hex : Color -> String
fromColor2Hex color =
    let
        src = String.toList "0123456789abcdef" |> Array.fromList
        fromSrc i = Array.get i src |> Maybe.withDefault '0'
        toHex v = String.fromList [fromSrc (v // 16), fromSrc (remainderBy 16 v)]
        hex = toHex color.r++toHex color.g++toHex color.b++toHex color.a
    in
    "#"++hex

moveColor : Settings -> Cfg.Conf -> String -> (SettingsColor -> SettingsColor) -> (SettingsColor -> String) -> (SettingsColor -> Bool) -> (Settings, Cfg.Conf)
moveColor osettings oconf ref modSettings modConf callConf =
    let
        nsettings = case ref of
            "bgCol" -> {osettings | bgCol = modSettings osettings.bgCol}
            _ -> osettings
        nconf =
            if callConf osettings.bgCol then case ref of
                "bgCol" -> {oconf | bgCol = modConf nsettings.bgCol}
                _ -> oconf
            else oconf
    in
    (nsettings, nconf)

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Vega - CUSTOMIZATION"
    , body =
        [ HS.toUnstyled
            (HS.div
                []
                [ HS.h2 [HA.css [C.textAlign C.center]] [HS.text "VEGA - Customizer"]
                , viewDateSelector model.time model.zone
                , viewLink model.url model.conf
                , HS.div
                    [HA.css [C.maxWidth (C.px 1400), C.margin C.auto]]
                    [ viewClockCustom model.conf model.time model.zone
                    , viewClockSettings model.conf model.settings
                    ]
                , HS.div [HA.css [C.property "clear" "both"]] [HS.text ""]
                ]
            )
            ]
    }

viewDateSelector : Time.Posix -> Time.Zone -> HS.Html Msg
viewDateSelector time zone =
    let
        btnCss =
            [ C.border3 (C.px 1) C.solid (C.hex "#fff")
            , C.backgroundColor (C.hex "#fff")
            , C.padding2 (C.px 3) (C.px 2)
            ]
        option a s = HS.option
            [HA.value (String.fromInt a), HA.selected s]
            [HS.text ((if a < 10 then "0" else "")++String.fromInt a)]
        sel max v m = HS.select
            [HA.css btnCss, HE.onInput m]
            (List.map (\a -> option a (a == v)) (List.range 0 max))
        --
        --
        timeBlock v r max =
            [ HS.button [HA.css btnCss] [HS.text "◀"]
            , sel max v (SelectTime r)
            , HS.button [HA.css btnCss] [HS.text "▶"]
            ]
        --
    in
    HS.div
        [ HA.css
            [ C.margin2 (C.px 10) C.auto
            , C.border3 (C.px 2) C.solid (C.hex "#000")
            , C.padding (C.px 10)
            , C.textAlign C.center
            , C.maxWidth (C.px 500)
            ]
        ]
        [ HS.p [] [] -- IDEA : YYYY/MM/DD
        , HS.p []
            (timeBlock (Time.toHour zone time) "hour" 23
            ++(HS.label [] [HS.text ":"]::timeBlock (Time.toMinute zone time) "minute" 59)
            ++(HS.label [] [HS.text ":"]::timeBlock (Time.toSecond zone time) "second" 59))
        , HS.p [] [] -- IDEA : | <- | select | -> |
        ]

viewLink : Url.Url -> Cfg.Conf -> HS.Html Msg
viewLink url conf =
    let
        surl = Url.toString url
        inter = if String.contains ".elm" surl then "/Main.elm?" else "/index.html?"
        linkViewer =
            case String.indices "/" surl |> List.reverse |> List.head of
                Just idx -> String.left idx surl ++ inter ++ buildInfos conf
                Nothing -> "Error in the url"
    in
    HS.div
        [HA.css [C.textAlign C.center, C.marginTop (C.px 20), C.marginBottom (C.px 20)]]
        [ HS.a
            [ HA.href linkViewer
            , HA.target "blank"
            , HA.css
                [ C.color (C.hex "#000")
                , C.fontWeight C.bold
                , C.fontSize (C.em 1.3)
                ]
            ]
            [HS.text ("Go to viewer: "++linkViewer)]
        ]

viewClockCustom : Cfg.Conf -> Time.Posix -> Time.Zone -> HS.Html Msg
viewClockCustom conf time zone =
    let
        (rWidth, rHeight) = Tuple.mapBoth toFloat toFloat (Cfg.screenValues conf.screen)
        margL coeff = if conf.screen == Cfg.Landscape then 10 else coeff * 50
    in
    HS.div
        [ HA.css
            [ C.margin (C.px 10)
            , C.marginLeft (C.px (margL 2))
            , C.backgroundColor (C.hex conf.bgCol)
            , C.width (C.px (rWidth*200))
            , C.height (C.px (rHeight*200))
            , C.float C.left
            , CM.withMedia
                [CM.all [CM.maxWidth (C.px 700)]]
                [C.property "float" "none", C.marginRight C.auto, C.marginLeft C.auto]
            , CM.withMedia
                [CM.all [CM.maxWidth (C.px 950)]]
                [ C.width (C.px (rWidth*100))
                , C.height (C.px (rHeight*100))
                , C.marginLeft (C.px (margL 1))
                ]
            , CM.withMedia
                [CM.all [CM.maxWidth (C.px 1200)]]
                [ C.width (C.px (rWidth*150))
                , C.height (C.px (rHeight*150))
                , C.marginLeft (C.px (margL 1.5))
                ]
            ]
        ]
        [ S.svg
            [ SA.css
                [ C.width (C.pct 100)
                , C.height (C.pct 100)
                ]
            , SA.viewBox ("0 0 "++ftos rWidth++"00"++" "++ftos rHeight++"00")
            ]
            (getClock conf time zone)
        ]

viewClockSettings : Cfg.Conf -> Settings -> HS.Html Msg
viewClockSettings conf settings =
    let
        subtitle subttl = HS.h4 [HA.css [C.marginLeft (C.px 20)]] [HS.text subttl]
        -- SCREEN =========================================
        orientOpts =
            [ {name = "landscape", value = "landscape", sel = conf.screen == Cfg.Landscape}
            , {name = "portrait", value = "portrait", sel = conf.screen == Cfg.Portrait}
            ]
        -- SECONDS ========================================
        --
        --
        --
    in
    HS.div
        [ HA.css
            [ C.margin (C.px 10)
            , C.padding (C.px 3)
            , C.float C.right
            , C.width (C.calc (C.pct 100) C.minus (C.px 720))
            , C.border3 (C.px 2) C.solid (C.hex "#000")
            , C.boxSizing C.borderBox
            , C.maxWidth (C.px 650)
            , CM.withMedia
                [CM.all [CM.maxWidth (C.px 700)]]
                [C.property "float" "none", C.width C.auto]
            , CM.withMedia
                [CM.all [CM.maxWidth (C.px 950)]]
                [C.width (C.calc (C.pct 100) C.minus (C.px 360))]
            , CM.withMedia
                [CM.all [CM.maxWidth (C.px 1200)]]
                [C.width (C.calc (C.pct 100) C.minus (C.px 540))]
            ]
        ]
        [ HS.h3 [HA.css [C.margin (C.px 5), C.marginLeft (C.px 10)]] [HS.text "Settings"]
        -- SCREEN ===================================================
        , subtitle "Screen settings"
        , viewOptionSelector "Orientation" SetOrientation orientOpts
        , viewColorSelector "Background color" "bgCol" conf.bgCol settings.bgCol
        -- SECONDS ==================================================
        , subtitle "Seconds settings"
        --
        --
        --
        --
        ]

viewSettingsBlock : String -> List (HS.Html Msg) -> HS.Html Msg
viewSettingsBlock title content =
    HS.p
        [HA.css [C.margin (C.px 4)]]
        ( HS.label
            [HA.css [C.marginRight (C.px 5), C.width (C.px 160), C.display C.inlineBlock]]
            [HS.text title]::content
        )

viewOptionSelector : String -> (String -> Msg) -> List {name : String, value : String, sel : Bool} -> HS.Html Msg
viewOptionSelector title msg options =
    let
        option name value sel = HS.option
            [HA.value value, HA.selected sel] [HS.text name]
    in
    viewSettingsBlock title
        [ HS.select
            [ HA.css [C.backgroundColor (C.hex "#fff"), C.border3 (C.px 1) C.solid (C.hex "#fff")]
            , HE.onInput msg
            ]
            (List.map (\o -> option o.name o.value o.sel) options)
        ]

viewColorSelector : String -> String -> String -> SettingsColor -> HS.Html Msg
viewColorSelector title ref hex settcol =
    let
        btn_width = 55
        btn_col c = fromColor2Hex c
        labCss stitle svalue = HS.label
            [HA.css [C.width (C.px 70), C.display C.inlineBlock]]
            [HS.text (stitle++": "++String.fromInt svalue)]
        sliCss sref usref val repcol = HS.input
            [ HA.css
                [ C.width (C.pct 80)
                , C.maxWidth (C.px 256)
                , C.backgroundColor (C.hex repcol)
                , C.property "appearance" "none"
                , C.property "-webkit-appearance" "none"
                , C.height (C.px 6)
                , C.marginBottom (C.px 12)
                ]
            , HA.type_ "range"
            , HA.min "0"
            , HA.max "255"
            , HA.value (String.fromInt val)
            , HE.onInput (MoveColorSlider (sref++"-"++usref))
            ] []
        btnCss col =
            [ C.width (C.px btn_width)
            , C.backgroundColor (C.hex (btn_col col))
            , C.color (C.hex (btn_col col))
            , C.marginLeft (C.px 15)
            , C.border3 (C.px 2) C.solid (C.hex "#fff")
            ]
        sliders =
            if settcol.open then
                [ HS.button
                    [ HA.css (btnCss settcol.c2)
                        , HE.onClick (SelectColorSlider ref)
                    ]
                    [HS.text "color"]
                , HS.br [] [], labCss "R" settcol.c2.r, sliCss ref "r" settcol.c2.r "#f00"
                , HS.br [] [], labCss "G" settcol.c2.g, sliCss ref "g" settcol.c2.g "#0f0"
                , HS.br [] [], labCss "B" settcol.c2.b, sliCss ref "b" settcol.c2.b "#00f"
                , HS.br [] [], labCss "a" settcol.c2.a, sliCss ref "a" settcol.c2.a "#aaa"
                ]
            else []
    in
    viewSettingsBlock title
        ( HS.button
            [ HA.css (btnCss settcol.c1)
            , HE.onClick (ToggleColorSliders ref)
            ]
            [HS.text "color"]
        ::sliders)
