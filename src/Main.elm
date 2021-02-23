module Main exposing (main)

import Browser
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , alignLeft
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , padding
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
import Html exposing (Html)
import Html.Attributes
import Random


type alias Model =
    { percentage : Int
    , actualPercentage : Float
    }


type Msg
    = Rnd Int
    | RandomNumberGenerated Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( { percentage = 50, actualPercentage = 0.0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rnd percentage ->
            ( { model | percentage = percentage }, Random.generate RandomNumberGenerated <| Random.float 0.0 100.0 )

        RandomNumberGenerated rnd ->
            ( { model | actualPercentage = rnd }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column [ width fill, height fill, spacing 2 ]
            [ row [ width fill, height fill, spacing 2 ]
                [ decide model ]
            , row [ width fill, height fill, spacing 2 ]
                [ button 25 (rgb255 142 202 230)
                , button 33 (rgb255 33 158 188)
                , button 50 (rgb255 144 190 109)
                , button 67 (rgb255 249 199 79)
                , button 75 (rgb255 249 65 68)
                ]
            , el [ centerX, padding 10 ] <|
                Element.link []
                    { url = "https://github.com/battermann/randomizer"
                    , label = row [] [ Element.html (Html.div [] [ Html.i [ Html.Attributes.class "fab fa-github" ] [] ]), text " Source Code" ]
                    }
            ]


decide : Model -> Element Msg
decide model =
    if toFloat model.percentage < model.actualPercentage then
        no model.actualPercentage model.percentage

    else
        yes model.actualPercentage model.percentage


yes : Float -> Int -> Element Msg
yes actualPercentage percentage =
    result actualPercentage percentage "YES" green


no : Float -> Int -> Element Msg
no actualPercentage percentage =
    result actualPercentage percentage "NO" red


roundPercentag : Float -> Float
roundPercentag v =
    toFloat (round (v * 10)) / 10


result : Float -> Int -> String -> Color -> Element Msg
result actual percentage txt color =
    column
        [ Background.color color
        , Border.rounded 3
        , width fill
        , height fill
        , userSelectNone
        ]
        [ el [ alignLeft, centerY, paddingXY 10 0 ] (text ("rnd: " ++ String.fromFloat (roundPercentag actual)))
        , el [ centerX, centerY, spacing 10 ] (row [ Font.size 32 ] [ text (String.fromInt percentage ++ " % "), text txt ])
        ]


button : Int -> Color -> Element Msg
button percentage color =
    Input.button
        [ Background.color color
        , Border.rounded 3
        , width fill
        , height fill
        , userSelectNone
        ]
        { onPress = Just (Rnd percentage)
        , label =
            el
                [ width fill ]
                (el [ centerX, centerY, Font.size 32 ] (text (String.fromInt percentage ++ " %")))
        }


red : Color
red =
    rgb255 255 0 0


green : Color
green =
    rgb255 0 128 0


userSelectNone : Attribute Msg
userSelectNone =
    htmlAttribute <| Html.Attributes.style "user-select" "none"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
