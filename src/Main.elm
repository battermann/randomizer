module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
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
import Html
import Html.Attributes
import Random
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)



---- MODEL ----


type alias Frequencies =
    { first : Int
    , second : Int
    , third : Int
    , fourth : Int
    , fifth : Int
    }


type alias Model =
    { frequency : Int
    , randomValue : Float
    , key : Nav.Key
    , url : Url
    , frequencies : Frequencies
    }


type Msg
    = Rnd Int
    | RandomNumberGenerated Float
    | ChangedUrl Url
    | ClickedLink UrlRequest



---- ROUTING ----


type Route
    = Home
    | Custom Int Int Int Int Int


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Custom (Parser.int </> Parser.int </> Parser.int </> Parser.int </> Parser.int)
        ]



---- UPDATE ----


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initialModel =
            { frequency = 50
            , randomValue = 0.0
            , key = key
            , url = url
            , frequencies =
                { first = 25
                , second = 33
                , third = 50
                , fourth = 67
                , fifth = 75
                }
            }
    in
    case Parser.parse routeParser url of
        Nothing ->
            ( initialModel, Cmd.none )

        Just Home ->
            ( initialModel, Cmd.none )

        Just (Custom first second third fourth fifth) ->
            ( { frequency = third
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
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rnd frequency ->
            ( { model | frequency = frequency }, Random.generate RandomNumberGenerated <| Random.float 0.0 100.0 )

        RandomNumberGenerated rnd ->
            ( { model | randomValue = rnd }, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Randomiz3r"
    , body =
        [ Element.layout [] <|
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
                , el [ centerX, padding 10 ] <|
                    Element.link []
                        { url = "https://github.com/battermann/randomizer"
                        , label = row [] [ Element.html (Html.div [] [ Html.i [ Html.Attributes.class "fab fa-github" ] [] ]), text " Source Code" ]
                        }
                ]
        ]
    }


decide : Model -> Element Msg
decide model =
    if toFloat model.frequency <= model.randomValue then
        no model.randomValue model.frequency

    else
        yes model.randomValue model.frequency


yes : Float -> Int -> Element Msg
yes randomValue frequency =
    result randomValue frequency "YES" green


no : Float -> Int -> Element Msg
no randomValue frequency =
    result randomValue frequency "NO" red


roundPercentag : Float -> Float
roundPercentag v =
    toFloat (round (v * 10)) / 10


result : Float -> Int -> String -> Color -> Element Msg
result actual frequency txt color =
    column
        [ Background.color color
        , Border.rounded 3
        , width fill
        , height fill
        , userSelectNone
        ]
        [ el [ alignLeft, centerY, paddingXY 10 0 ] (text ("rnd: " ++ String.fromFloat (roundPercentag actual)))
        , el [ centerX, centerY, spacing 10 ] (row [ Font.size 32 ] [ text (String.fromInt frequency ++ " % "), text txt ])
        ]


button : Int -> Color -> Element Msg
button frequency color =
    Input.button
        [ Background.color color
        , Border.rounded 3
        , width fill
        , height fill
        , userSelectNone
        ]
        { onPress = Just (Rnd frequency)
        , label =
            el
                [ width fill ]
                (el [ centerX, centerY, Font.size 32 ] (text (String.fromInt frequency ++ " %")))
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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
