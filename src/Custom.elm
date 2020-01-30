module Custom exposing(main)

import Clock exposing(getClock, ftos)
import Config as Cfg
import Parsing exposing(parseUrl)

import Browser
import Browser.Navigation as BN
import Css as C
import Css.Media as CM
import Html.Styled as HS
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Task
import Time
import Url

--import Debug

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

type alias Model =
    { key : BN.Key
    , url : Url.Url
    , zone : Time.Zone
    , time : Time.Posix
    , conf : Cfg.Conf
    }

initModel : BN.Key -> Url.Url -> Model
initModel key url =
    Model
        key
        url
        Time.utc
        (Time.millisToPosix 0)
        (parseUrl url)

init : () -> Url.Url -> BN.Key -> (Model, Cmd Msg)
init _ url key = (initModel key url, Task.perform SetZone Time.here)

-- UPDATE

type Msg
    = SetZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    -- time messages
    --
    -- settings messages
    | SetOrientation String
    --
    --
    --
    | Tst

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetZone nzone -> ({model | zone = nzone}, Cmd.none)
        LinkClicked _ -> (model, Cmd.none)
        UrlChanged url -> ({model | url = url, conf = parseUrl url}, Cmd.none)
        -- time update
        --
        -- IDEA : date conf
        --
        -- settings update
        SetOrientation _ -> --nscreen
            let
                {-
                ascreen = case nscreen of
                    "landscape" -> Cfg.Landscape
                    "portrait" -> Cfg.Portrait
                    _ -> model.conf.screen
                oconf = model.conf
                -}
                --
                _ = ""
                --
                --buildUrl ({oconf | screen = ascreen})
                --
                --
                -- IDEA : update the custom link
                --
            in
            --({model | conf = nconf}, Cmd.none)
            --(model, BN.pushUrl model.key (Url.toString url))
            --
            (model, Cmd.none)
            --
        --
        --
        Tst ->
            let
                --
                surl = Url.toString model.url
                nurl = if model.conf.screen == Cfg.Landscape then surl++"?1::" else surl++"?0::"
                --
            in
            (model, BN.pushUrl model.key nurl)
        --

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- HELPERS

settingBlock : String -> List (HS.Html Msg) -> HS.Html Msg
settingBlock title content =
    HS.p
        []
        (HS.label [HA.css [C.marginRight (C.px 5)]] [HS.text title]::content)

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
                , viewClockCustom model.conf model.time model.zone
                , viewClockSettings model.conf
                , HS.div [HA.css [C.property "clear" "both"]] [HS.text ""]
                ]
            )
            ]
    }

viewDateSelector : Time.Posix -> Time.Zone -> HS.Html Msg
viewDateSelector _ _ =
    let
        --
        --
        _ = ""
        --
    in
    HS.div
        [ HA.css
            [ C.margin (C.px 10)
            , C.border3 (C.px 2) C.solid (C.hex "#000")
            , C.padding (C.px 10)
            , C.textAlign C.center
            ]
        ]
        [ HS.p [] [] -- IDEA : YYYY/MM/DD
        , HS.p []
        --
        -- IDEA : HH:MM:SS
        --
            [ HS.label [] [HS.text ":"]
            , HS.input [] []
            , HS.label [] [HS.text ":"]
            --
            , HS.input
                []
                []
            --
            --
            ]
        , HS.p [] [] -- IDEA : | <- | select | -> |
        ]

viewLink : Url.Url -> Cfg.Conf -> HS.Html Msg
viewLink _ _ =
    let
        {-
        surl = Url.toString url
        inter = if String.contains ".elm" surl then "/Main.elm?" else "/index.html?"
        linkClck =
            case String.indices "/" surl |> List.reverse |> List.head of
                Just idx -> (String.left idx surl)++inter++(buildInfos conf)
                Nothing -> "Error in the url"
        -}
        --
        _ = ""
        --
    in
    HS.div
        [HA.css [C.textAlign C.center]]
        [ HS.a
            [] []
            --[HA.href linkClck, HA.target "blank"]
            --[text linkClck]
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

viewClockSettings : Cfg.Conf -> HS.Html Msg
viewClockSettings conf =
    HS.div
        [ HA.css
            [ C.margin (C.px 10)
            , C.padding (C.px 3)
            , C.float C.right
            , C.width (C.calc (C.pct 100) C.minus (C.px 720))
            , C.border3 (C.px 2) C.solid (C.hex "#000")
            , C.boxSizing C.borderBox
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
        , settingBlock "Orientation"
            [ HS.select
                [HE.onInput SetOrientation]
                [ HS.option
                    [HA.value "landscape", HA.selected (conf.screen == Cfg.Landscape)]
                    [HS.text "landscape"]
                , HS.option
                    [HA.value "portrait", HA.selected (conf.screen == Cfg.Portrait)]
                    [HS.text "portrait"]
                ]
            ]
        --
        --
        , HS.button [HE.onClick Tst] [HS.text "test btn"]
        --
        --
        ]
