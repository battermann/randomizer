module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element
    exposing
        ( centerX
        , column
        , fill
        , height
        , padding
        , row
        , spacing
        , text
        , width
        )
import Frequencies
import Html
import Html.Attributes
import Timer
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)



---- MODEL ----


type Page
    = FrequenciesPage Frequencies.Model
    | TimerPage Timer.Model


type alias Model =
    { page : Page
    , key : Nav.Key
    , url : Url
    }


type Msg
    = FrequenciesMsg Frequencies.Msg
    | TimerMsg Timer.Msg
    | UrlChanged Url
    | LinkClicked UrlRequest



---- ROUTING ----


type Route
    = DefaultFrequencies
    | CustomFrequencies Int Int Int Int Int
    | Timer Int


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Timer (Parser.s "timer" </> Parser.int)
        , Parser.map (Timer 0) (Parser.s "timer")
        , Parser.map DefaultFrequencies Parser.top
        , Parser.map CustomFrequencies (Parser.int </> Parser.int </> Parser.int </> Parser.int </> Parser.int)
        ]


parseUrl : Url.Url -> Nav.Key -> Model -> ( Model, Cmd Msg )
parseUrl url key model =
    case Parser.parse routeParser url of
        Nothing ->
            ( model, Cmd.none )

        Just DefaultFrequencies ->
            ( model, Cmd.none )

        Just (CustomFrequencies first second third fourth fifth) ->
            ( { model | page = FrequenciesPage (Frequencies.init url key first second third fourth fifth) }, Cmd.none )

        Just (Timer sec) ->
            let
                ( m, c ) =
                    Timer.init url key sec
            in
            ( { model | page = TimerPage m }, c |> Cmd.map TimerMsg )



---- UPDATE ----


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let
        model =
            { page = FrequenciesPage (Frequencies.init url key 25 33 50 67 75)
            , key = key
            , url = url
            }
    in
    parseUrl url key model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            parseUrl url model.key model

        FrequenciesMsg subMsg ->
            case model.page of
                FrequenciesPage subModel ->
                    let
                        ( m, c ) =
                            Frequencies.update subMsg subModel
                    in
                    ( { model | page = FrequenciesPage m }, c |> Cmd.map FrequenciesMsg )

                TimerPage _ ->
                    ( model, Cmd.none )

        TimerMsg subMsg ->
            case model.page of
                TimerPage subModel ->
                    let
                        ( m, c ) =
                            Timer.update subMsg subModel
                    in
                    ( { model | page = TimerPage m }, c |> Cmd.map TimerMsg )

                FrequenciesPage _ ->
                    ( model, Cmd.none )



---- VIEW ----


pageView : { a | page : Page } -> Element.Element Msg
pageView model =
    case model.page of
        FrequenciesPage m ->
            Frequencies.view m |> Element.map FrequenciesMsg

        TimerPage m ->
            Timer.view m |> Element.map TimerMsg


view : Model -> Document Msg
view model =
    { title = "Randomiz3r"
    , body =
        [ Element.layout [] <|
            column [ width fill, height fill, spacing 2 ]
                [ pageView model
                , row [ centerX ]
                    [ Element.link [ padding 10 ]
                        { url = "https://github.com/battermann/randomizer"
                        , label = row [] [ Element.html (Html.div [] [ Html.i [ Html.Attributes.class "fab fa-github" ] [] ]), text " Source Code" ]
                        }
                    ]
                ]
        ]
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        FrequenciesPage m ->
            Frequencies.subscriptions m |> Sub.map FrequenciesMsg

        TimerPage m ->
            Timer.subscription m |> Sub.map TimerMsg



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
