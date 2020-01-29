module Custom exposing(main)

import Clock exposing(..)
import Config exposing(..)
import Parsing exposing(..)

import Browser
import Browser.Navigation as BN
import Css exposing(..)
import Css.Media as CM
import Html.Styled exposing(..)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Task
import Time
import Url

--import Debug

-- MAIN

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
    , conf : Conf
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
        LinkClicked urlRequest -> (model, Cmd.none)
        UrlChanged url -> ({model | url = url, conf = parseUrl url}, Cmd.none)
        -- time update
        --
        -- TODO : date conf
        --
        -- settings update
        SetOrientation nscreen ->
            let
                ascreen = case nscreen of
                    "landscape" -> Landscape
                    "portrait" -> Portrait
                    _ -> model.conf.screen
                oconf = model.conf
                --buildUrl ({oconf | screen = ascreen})
                --
                --
                -- TODO : update the custom link
                --
            in
            --({model | conf = nconf}, Cmd.none)
            (model, BN.pushUrl model.key (Url.toString url))
        --
        --
        Tst ->
            let
                --
                surl = Url.toString model.url
                nurl = if model.conf.screen == Landscape then surl++"?1::" else surl++"?0::"
                --
            in
            (model, BN.pushUrl model.key nurl)
        --

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HELPERS

settingBlock : String -> List (Html Msg) -> Html Msg
settingBlock title content =
    p
        []
        (label [HA.css [marginRight (px 5)]] [text title]::content)

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Vega - CUSTOMIZATION"
    , body =
        [ toUnstyled
            (div
                []
                [ h2 [HA.css [textAlign center]] [text "VEGA - Customizer"]
                , viewDateSelector model.time model.zone
                , viewLink model.url model.conf
                , viewClockCustom model.conf model.time model.zone
                , viewClockSettings model.conf
                , div [HA.css [property "clear" "both"]] [text ""]
                ]
            )
            ]
    }

viewDateSelector : Time.Posix -> Time.Zone -> Html Msg
viewDateSelector time zone =
    let
        --
        --
        _ = ""
        --
    in
    div
        [ HA.css
            [ margin (px 10)
            , border3 (px 2) solid (hex "#000")
            , padding (px 10)
            , textAlign center
            ]
        ]
        [ p [] [] -- TODO : YYYY/MM/DD
        , p []
        --
        -- TODO : HH:MM:SS
        --
            [ label [] [text ":"]
            , input [] []
            , label [] [text ":"]
            --
            , input
                []
                []
            --
            --
            ]
        , p [] [] -- TODO : | <- | select | -> |
        ]

viewLink : Url.Url -> Conf -> Html Msg
viewLink url conf =
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
    div
        [HA.css [textAlign center]]
        [ a
            [] []
            --[HA.href linkClck, HA.target "blank"]
            --[text linkClck]
        ]

viewClockCustom : Conf -> Time.Posix -> Time.Zone -> Html Msg
viewClockCustom conf time zone =
    let
        (rWidth, rHeight) = Tuple.mapBoth toFloat toFloat (screenValues conf.screen)
        margL coeff = if conf.screen == Landscape then 10 else coeff * 50
    in
    div
        [ HA.css
            [ margin (px 10)
            , marginLeft (px (margL 2))
            , backgroundColor (hex conf.bgCol)
            , width (px (rWidth*200))
            , height (px (rHeight*200))
            , float left
            , CM.withMedia
                [CM.all [CM.maxWidth (px 700)]]
                [property "float" "none", marginRight auto, marginLeft auto]
            , CM.withMedia
                [CM.all [CM.maxWidth (px 950)]]
                [ width (px (rWidth*100))
                , height (px (rHeight*100))
                , marginLeft (px (margL 1))
                ]
            , CM.withMedia
                [CM.all [CM.maxWidth (px 1200)]]
                [ width (px (rWidth*150))
                , height (px (rHeight*150))
                , marginLeft (px (margL 1.5))
                ]
            ]
        ]
        [ S.svg
            [ SA.css
                [ width (pct 100)
                , height (pct 100)
                ]
            , SA.viewBox ("0 0 "++(ftos rWidth)++"00"++" "++(ftos rHeight)++"00")
            ]
            (getClock conf time zone)
        ]

viewClockSettings : Conf -> Html Msg
viewClockSettings conf =
    div
        [ HA.css
            [ margin (px 10)
            , padding (px 3)
            , float right
            , width (calc (pct 100) minus (px 720))
            , border3 (px 2) solid (hex "#000")
            , boxSizing borderBox
            , CM.withMedia
                [CM.all [CM.maxWidth (px 700)]]
                [property "float" "none", width auto]
            , CM.withMedia
                [CM.all [CM.maxWidth (px 950)]]
                [width (calc (pct 100) minus (px 360))]
            , CM.withMedia
                [CM.all [CM.maxWidth (px 1200)]]
                [width (calc (pct 100) minus (px 540))]
            ]
        ]
        [ h3 [HA.css [margin (px 5), marginLeft (px 10)]] [text "Settings"]
        , settingBlock "Orientation"
            [ select
                [HE.onInput SetOrientation]
                [ option
                    [HA.value "landscape", HA.selected (conf.screen == Landscape)]
                    [text "landscape"]
                , option
                    [HA.value "portrait", HA.selected (conf.screen == Portrait)]
                    [text "portrait"]
                ]
            ]
        --
        --
        , button [HE.onClick Tst] [text "test btn"]
        --
        --
        ]
