module Main exposing (main)

import Browser exposing (Document, UrlRequest)
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
    | UrlChanged Url
    | LinkClicked UrlRequest



---- ROUTING ----


type Route
    = Default
    | Custom Int Int Int Int Int


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Default Parser.top
        , Parser.map Custom (Parser.int </> Parser.int </> Parser.int </> Parser.int </> Parser.int)
        ]


parseUrl : Url.Url -> Nav.Key -> Model -> Model
parseUrl url key model =
    case Parser.parse routeParser url of
        Nothing ->
            model

        Just Default ->
            model

        Just (Custom first second third fourth fifth) ->
            { model
                | selectedFrequency = Nothing
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



---- UPDATE ----


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let
        model =
            { selectedFrequency = Nothing
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
            , clickedFrequency = Nothing
            }
    in
    ( parseUrl url key model, Cmd.none )


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

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( parseUrl url model.key model, Cmd.none )

        FrequencyClicked frequency ->
            ( { model | clickedFrequency = Just frequency, selectedFrequency = Nothing }, Cmd.none )



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
                , row [ centerX ]
                    [ Element.link [ padding 10 ]
                        { url = "https://github.com/battermann/randomizer"
                        , label = row [] [ Element.html (Html.div [] [ Html.i [ Html.Attributes.class "fab fa-github" ] [] ]), text " Source Code" ]
                        }
                    , el [ padding 10 ] (Element.html donate)
                    ]
                ]
        ]
    }


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


donate : Html.Html msg
donate =
    Html.form
        [ Html.Attributes.action "https://www.paypal.com/donate"
        , Html.Attributes.method "post"
        , Html.Attributes.target "_top"
        ]
        [ Html.input
            [ Html.Attributes.name "hosted_button_id"
            , Html.Attributes.type_ "hidden"
            , Html.Attributes.value "QJA3Y3DLGWVQ8"
            ]
            []
        , Html.text ""
        , Html.input
            [ Html.Attributes.alt "Donate with PayPal button"
            , Html.Attributes.attribute "border" "0"
            , Html.Attributes.name "submit"
            , Html.Attributes.src "https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif"
            , Html.Attributes.title "PayPal - The safer, easier way to pay online!"
            , Html.Attributes.type_ "image"
            ]
            []
        , Html.text ""
        , Html.img
            [ Html.Attributes.alt ""
            , Html.Attributes.attribute "border" "0"
            , Html.Attributes.attribute "height" "1"
            , Html.Attributes.src "https://www.paypal.com/en_DE/i/scr/pixel.gif"
            , Html.Attributes.attribute "width" "1"
            ]
            []
        , Html.text ""
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.clickedFrequency of
        Just frequency ->
            Browser.Events.onAnimationFrame (\_ -> FrequencySelected frequency)

        Nothing ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
