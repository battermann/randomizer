module Timer exposing (Model, Msg, init, subscription, update, view)

import Browser.Navigation as Nav
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , fill
        , height
        , htmlAttribute
        , width
        )
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Random
import Time
import Url exposing (Url)


type alias Model =
    { currentRnd : Int
    , nextRnd : Int
    , interval : Int
    , key : Nav.Key
    , url : Url
    }


init : Url.Url -> Nav.Key -> Int -> ( Model, Cmd Msg )
init url key interval =
    ( { currentRnd = 0
      , nextRnd = 0
      , interval = interval
      , key = key
      , url = url
      }
    , Random.generate RandomNumberGenerated <| Random.int 0 100
    )


type Msg
    = Tick
    | RandomNumberGenerated Int
    | Interval
    | Clicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Interval ->
            ( model, Random.generate RandomNumberGenerated <| Random.int 0 100 )

        Tick ->
            if model.currentRnd < model.nextRnd then
                ( { model | currentRnd = model.currentRnd + 1 }, Cmd.none )

            else if model.currentRnd > model.nextRnd then
                ( { model | currentRnd = model.currentRnd - 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        RandomNumberGenerated rnd ->
            ( { model | nextRnd = rnd }, Cmd.none )

        Clicked ->
            ( model, Random.generate RandomNumberGenerated <| Random.int 0 100 )


button : Model -> Element Msg
button model =
    Input.button
        [ Border.rounded 3
        , width fill
        , height fill
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
        { onPress = Just Clicked
        , label = Element.el [ height fill, width fill ] (Element.el [ centerX, centerY, Element.htmlAttribute <| Html.Attributes.style "font-size" "60vmin" ] (Element.text (model.currentRnd |> String.fromInt)))
        }


view : Model -> Element Msg
view =
    button


subscription : Model -> Sub Msg
subscription model =
    Sub.batch
        (Time.every 5.0 (always Tick)
            :: (if model.interval > 0 then
                    [ Time.every (model.interval * 1000 |> toFloat) (always Interval) ]

                else
                    []
               )
        )
