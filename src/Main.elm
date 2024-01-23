port module Main exposing (main)

import Browser
import Browser.Navigation
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Process
import Random
import Task
import Url.Builder
import Url.Parser
import Url.Parser.Query


port sendAudioCommand : ( String, Maybe Float ) -> Cmd msg


notesPerMelody : Int
notesPerMelody =
    6


type Key
    = White
    | Black
    | Gray


type alias Keyboard =
    List Key


defaultKeyboard : Keyboard
defaultKeyboard =
    [ White
    , Black
    , White
    , Black
    , White
    , White
    , Black
    , White
    , Black
    , White
    , Black
    , White
    ]


noteFrequency : Int -> Float
noteFrequency i =
    440 * 2 ^ ((toFloat i - 10) / 12)


white : Element.Color
white =
    Element.rgb 1 1 1


black : Element.Color
black =
    Element.rgb 0 0 0


blue : Element.Color
blue =
    Element.rgba 0.3 0.3 0.8 0.4


darkGray : Element.Color
darkGray =
    Element.rgb255 64 64 64


lightGray : Element.Color
lightGray =
    Element.rgb 0.8 0.8 0.8


shiftKeyboardRight : Int -> Keyboard -> Keyboard
shiftKeyboardRight n keyboard =
    if n < 1 then
        keyboard

    else
        shiftKeyboardRight (n - 1) (List.drop 1 keyboard ++ List.take 1 keyboard)


shiftKeyboardLeft : Int -> Keyboard -> Keyboard
shiftKeyboardLeft n keyboard =
    if n < 1 then
        keyboard

    else
        let
            l =
                List.length keyboard
        in
        shiftKeyboardLeft (n - 1) (List.drop (l - 1) keyboard ++ List.take (l - 1) keyboard)


type SynthState
    = Stopped
    | Playing (List Int)


type alias Model =
    { melody : List Int
    , keyboard : Keyboard
    , navigationKey : Browser.Navigation.Key
    , synthState : SynthState
    }


type Msg
    = Nop
    | ShiftKeyboardRight
    | ShiftKeyboardLeft
    | Generate
    | SetMelody (List Int)
    | PlayMelody


renderKeyLabel : List (Element.Attribute msg) -> Int -> Element.Element msg
renderKeyLabel attrs i =
    Element.el
        ([ Element.width (Element.px 30)
         , Element.height (Element.px 30)
         , Element.Border.rounded 15
         , Element.Border.solid
         , Element.Border.width 1
         , Element.centerX
         , Element.alignBottom
         , Element.moveUp 30
         ]
            ++ attrs
        )
        (Element.el [ Element.centerX, Element.centerY ] (Element.text (String.fromInt i)))


renderKeyboard : Keyboard -> Dict.Dict Int (List Int) -> Element.Element msg
renderKeyboard keyboard melody =
    let
        adjustedKeyboard =
            Gray :: keyboard ++ [ Gray ]
    in
    adjustedKeyboard
        |> List.indexedMap
            (\step key ->
                let
                    plays =
                        Dict.get step melody
                            |> Maybe.withDefault []
                in
                case key of
                    Gray ->
                        Element.el
                            [ Element.width (Element.fillPortion 1)
                            , Element.height (Element.px 400)
                            , Element.Border.solid
                            , Element.Border.width 1
                            , Element.Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
                            , Element.Background.color lightGray
                            ]
                            (Element.text "")

                    White ->
                        Element.column
                            [ Element.width (Element.fillPortion 1)
                            , Element.height (Element.px 400)
                            , Element.Border.solid
                            , Element.Border.width 1
                            , Element.Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
                            , Element.Background.color white
                            , Element.spacing 5
                            ]
                            (List.map (renderKeyLabel []) plays)

                    Black ->
                        let
                            keyEl =
                                Element.column
                                    [ Element.width (Element.px 50)
                                    , Element.height (Element.px 260)
                                    , Element.Border.solid
                                    , Element.Border.width 1
                                    , Element.Background.color black
                                    , Element.alignTop
                                    , Element.moveRight 27
                                    , Element.spacing 5
                                    , Element.Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 10, bottomRight = 10 }
                                    ]
                                    (List.map (renderKeyLabel [ Element.Border.color white, Element.Font.color white ]) plays)
                        in
                        Element.el
                            [ Element.width (Element.px 0)
                            , Element.height (Element.px 300)
                            , Element.onLeft keyEl
                            , Element.alignTop
                            ]
                            (Element.text "")
            )
        |> Element.row [ Element.width (Element.fillPortion 9) ]


renderLeftPanel : Model -> Element.Element Msg
renderLeftPanel _ =
    let
        attrs =
            [ Element.Border.solid
            , Element.Border.rounded 5
            , Element.Border.width 1
            , Element.width Element.fill
            , Element.height (Element.px 35)
            , Element.Font.color white
            , Element.centerX
            , Element.padding 5
            ]
    in
    Element.column
        [ Element.width Element.fill, Element.spacing 10, Element.padding 5 ]
        [ Element.Input.button attrs
            { onPress = Just ShiftKeyboardRight
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "△")
            }
        , Element.Input.button attrs
            { onPress = Just ShiftKeyboardLeft
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "▽")
            }
        ]


renderTopPanel : Model -> Element.Element Msg
renderTopPanel model =
    Element.row
        [ Element.width Element.fill, Element.padding 10, Element.spacing 20 ]
        [ Element.column
            [ Element.Font.color white
            , Element.height Element.fill
            , Element.Font.family [ Element.Font.typeface "Impact", Element.Font.sansSerif ]
            ]
            [ Element.text "СЛУЧАЙНАЯ", Element.text "МЕЛОДИЯ" ]
        , model.melody
            |> List.map
                (\step ->
                    Element.el
                        [ Element.Border.width 1
                        , Element.Border.color white
                        , Element.Font.color white
                        , Element.padding 10
                        , Element.Border.rounded 5
                        , Element.width (Element.fillPortion 1)
                        ]
                        (Element.el [ Element.centerX, Element.centerY ] (Element.text (String.fromInt step)))
                )
            |> Element.row [ Element.width (Element.fillPortion 8), Element.spacing 10, Element.height Element.fill ]
        , Element.Input.button
            [ Element.Border.width 1
            , Element.Border.color white
            , Element.Font.color white
            , Element.padding 10
            , Element.Border.rounded 5
            , Element.width (Element.fillPortion 1)
            , Element.Background.color blue
            ]
            { onPress = Just Generate, label = Element.el [ Element.centerX, Element.centerY ] (Element.text "♽") }
        ]


melodyByNote : List Int -> Dict.Dict Int (List Int)
melodyByNote melody =
    melody
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, s ) a ->
                Dict.update s
                    (\mV ->
                        case mV of
                            Just v ->
                                Just (v ++ [ i + 1 ])

                            Nothing ->
                                Just [ i + 1 ]
                    )
                    a
            )
            Dict.empty


view : Model -> Element.Element Msg
view model =
    Element.column
        [ Element.width (Element.fill |> Element.minimum 800 |> Element.maximum 960)
        , Element.centerX
        , Element.padding 10
        , Element.Background.color darkGray
        , Element.Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 30, bottomRight = 20 }
        ]
        [ renderTopPanel model
        , Element.row [ Element.width Element.fill ]
            [ renderLeftPanel model
            , renderKeyboard model.keyboard
                (melodyByNote model.melody)
            ]
        ]


pushUrl : Browser.Navigation.Key -> List Int -> Cmd Msg
pushUrl navKey melody =
    Browser.Navigation.pushUrl navKey (Url.Builder.relative [] [ Url.Builder.string "melody" (serializeMelody melody) ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate SetMelody
                (Random.list (notesPerMelody - 1) (Random.int 1 (List.length defaultKeyboard))
                    |> Random.map (\l -> 1 :: l)
                )
            )

        Nop ->
            ( model, Cmd.none )

        PlayMelody ->
            case model.synthState of
                Stopped ->
                    ( model, Cmd.none )

                Playing (i :: rest) ->
                    ( { model | synthState = Playing rest }
                    , Cmd.batch
                        [ sendAudioCommand ( "play", Just (noteFrequency i) )
                        , Task.perform (always PlayMelody) (Process.sleep 500)
                        ]
                    )

                Playing [] ->
                    ( { model | synthState = Stopped }, sendAudioCommand ( "stop", Nothing ) )

        SetMelody melody ->
            ( { model | melody = melody, synthState = Playing melody }
            , Cmd.batch
                [ pushUrl model.navigationKey melody
                , Task.perform (always PlayMelody) (Task.succeed ())
                ]
            )

        ShiftKeyboardLeft ->
            ( { model | keyboard = shiftKeyboardLeft 1 model.keyboard }, Cmd.none )

        ShiftKeyboardRight ->
            ( { model | keyboard = shiftKeyboardRight 1 model.keyboard }, Cmd.none )


normalizeMelody : List Int -> List Int
normalizeMelody melody =
    melody
        |> List.head
        |> Maybe.map
            (\first ->
                if first == 1 then
                    melody

                else
                    List.map (\i -> modBy (List.length defaultKeyboard) (i - first) + 1) melody
            )
        |> Maybe.withDefault melody


deserialzeMelody : String -> List Int
deserialzeMelody s =
    s
        |> String.split "-"
        |> List.take 6
        |> List.filterMap String.toInt
        |> normalizeMelody


serializeMelody : List Int -> String
serializeMelody melody =
    melody
        |> List.take 6
        |> List.map String.fromInt
        |> String.join "-"


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url navKey ->
                let
                    melody =
                        { url | path = "" }
                            |> Url.Parser.parse
                                (Url.Parser.query (Url.Parser.Query.string "melody"))
                            |> Maybe.andThen identity
                            |> Maybe.map deserialzeMelody
                            |> Maybe.withDefault []
                in
                ( { melody = melody
                  , keyboard = defaultKeyboard
                  , navigationKey = navKey
                  , synthState = Stopped
                  }
                , pushUrl navKey melody
                )
        , view =
            view
                >> (\model ->
                        { title = "Случайная мелодия"
                        , body = [ Element.layout [ Element.width Element.fill ] model ]
                        }
                   )
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = always Nop
        , onUrlRequest = always Nop
        }
