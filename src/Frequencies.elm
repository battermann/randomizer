module Frequencies exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Browser.Navigation as Nav
import Element
    exposing
        ( Color
        , Element
        , alignLeft
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , paddingXY
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Random
import Url exposing (Url)



---- MODEL ----


type alias Frequencies =
    { first : Int
    , second : Int
    , third : Int
    , fourth : Int
    , fifth : Int
    }


type alias Model =
    { selectedFrequency : Maybe Int
    , randomValue : Float
    , key : Nav.Key
    , url : Url
    , frequencies : Frequencies
    , clickedFrequency : Maybe Int
    }


type Msg
    = FrequencySelected Int
    | FrequencyClicked Int
    | RandomNumberGenerated Float



---- UPDATE ----


init : Url.Url -> Nav.Key -> Int -> Int -> Int -> Int -> Int -> Model
init url key first second third fourth fifth =
    { selectedFrequency = Nothing
    , randomValue = 0.0
    , key = key
    , url = url
    , frequencies =
        { first = first
        , second = second
        , third = third
        , fourth = fourth
        , fifth = fifth
        }
    , clickedFrequency = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrequencySelected selectedFrequency ->
            ( { model
                | selectedFrequency = Just selectedFrequency
                , clickedFrequency = Nothing
              }
            , Random.generate RandomNumberGenerated <| Random.float 0.0 100.0
            )

        RandomNumberGenerated rnd ->
            ( { model | randomValue = rnd }, Cmd.none )

        FrequencyClicked frequency ->
            ( { model | clickedFrequency = Just frequency, selectedFrequency = Nothing }, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, height fill, spacing 2 ]
        [ row [ width fill, height fill, spacing 2 ]
            [ decide model ]
        , row [ width fill, height fill, spacing 2 ]
            [ button model.frequencies.first (rgb255 142 202 230)
            , button model.frequencies.second (rgb255 33 158 188)
            , button model.frequencies.third (rgb255 144 190 109)
            , button model.frequencies.fourth (rgb255 249 199 79)
            , button model.frequencies.fifth (rgb255 249 65 68)
            ]
        ]


decide : Model -> Element Msg
decide model =
    let
        red =
            rgb255 255 0 0

        green =
            rgb255 0 128 0
    in
    case model.selectedFrequency of
        Just frequency ->
            if toFloat frequency <= model.randomValue then
                result model.randomValue frequency "NO" red

            else
                result model.randomValue frequency "YES" green

        Nothing ->
            el
                [ Background.color (rgb255 128 128 128)
                , Border.rounded 3
                , width fill
                , height fill
                , htmlAttribute <| Html.Attributes.style "user-select" "none"
                ]
                Element.none


roundPercentag : Float -> Float
roundPercentag v =
    toFloat (round (v * 10)) / 10


result : Float -> Int -> String -> Color -> Element Msg
result actual selectedFrequency txt color =
    column
        [ Background.color color
        , Border.rounded 3
        , width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        , htmlAttribute <| Html.Attributes.style "animation" "fadeIn 1s"
        ]
        [ el [ alignLeft, paddingXY 10 10 ] (text ("rnd: " ++ String.fromFloat (roundPercentag actual)))
        , el [ centerX, centerY, spacing 10 ] (row [ Font.size 32 ] [ text (String.fromInt selectedFrequency ++ " % "), text txt ])
        ]


button : Int -> Color -> Element Msg
button selectedFrequency color =
    Input.button
        [ Background.color color
        , Border.rounded 3
        , width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
        { onPress = Just (FrequencyClicked selectedFrequency)
        , label =
            el
                [ width fill ]
                (el [ centerX, centerY, Font.size 32 ] (text (String.fromInt selectedFrequency)))
        }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.clickedFrequency of
        Just frequency ->
            Browser.Events.onAnimationFrame (\_ -> FrequencySelected frequency)

        Nothing ->
            Sub.none
