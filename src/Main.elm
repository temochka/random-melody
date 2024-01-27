port module Main exposing (main)

import Browser
import Browser.Navigation
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Region
import Html.Attributes
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


blueGlow : Element.Color
blueGlow =
    Element.rgb255 235 245 250


blue : Element.Color
blue =
    Element.rgb255 170 205 252


darkGray : Element.Color
darkGray =
    Element.rgb255 64 64 64


lightGray : Element.Color
lightGray =
    Element.rgb 0.8 0.8 0.8


shiftKeyboard : Int -> Keyboard -> Keyboard
shiftKeyboard n keyboard =
    let
        rightShift =
            if n < 0 then
                List.length keyboard + n

            else
                n
    in
    if n == 0 then
        keyboard

    else
        shiftKeyboard (rightShift - 1) (List.drop 1 keyboard ++ List.take 1 keyboard)


type SynthState
    = Stopped
    | Playing (List Int)


type alias Model =
    { melody : List Int
    , keyboard : Keyboard
    , shift : Int
    , navigationKey : Browser.Navigation.Key
    , synthState : SynthState
    , showHelp : Bool
    }


type Msg
    = Nop
    | ShiftKeyboard Int
    | Generate
    | PlayMelody
    | Reset Int (List Int)
    | ToggleHelp


renderKeyLabel : List (Element.Attribute msg) -> Maybe Int -> Int -> Element.Element msg
renderKeyLabel attrs playedKey i =
    Element.el
        ([ Element.width (Element.px 30)
         , Element.height (Element.px 30)
         , Element.Border.rounded 15
         , Element.Border.solid
         , Element.Border.width 1
         , Element.centerX
         , Element.alignBottom
         , Element.moveUp 30
         , if playedKey == Just i then
            Element.Background.color blue

           else
            Element.Background.color (Element.rgba 255 255 255 0)
         ]
            ++ attrs
        )
        (Element.el [ Element.centerX, Element.centerY ] (Element.text (String.fromInt i)))


renderKeyboard : Keyboard -> Dict.Dict Int (List Int) -> Maybe Int -> Element.Element msg
renderKeyboard keyboard melody playedKey =
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
                            (List.map (renderKeyLabel [] playedKey) plays)

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
                                    (List.map (renderKeyLabel [ Element.Border.color white, Element.Font.color white ] playedKey) plays)
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
renderLeftPanel model =
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
        [ Element.width (Element.px 65), Element.spacing 10, Element.paddingEach { top = 0, bottom = 0, left = 0, right = 5 } ]
        [ Element.Input.button attrs
            { onPress = Just (ShiftKeyboard 1)
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "△")
            }
        , Element.Input.button attrs
            { onPress = Just (ShiftKeyboard -model.shift)
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "C")
            }
        , Element.Input.button attrs
            { onPress = Just (ShiftKeyboard -1)
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "▽")
            }
        ]


renderRightPanel : Model -> Element.Element Msg
renderRightPanel _ =
    let
        attrs =
            [ Element.Border.solid
            , Element.Border.rounded 5
            , Element.Border.width 1
            , Element.width Element.fill
            , Element.height (Element.px 35)
            , Element.Font.color white
            , Element.centerX
            , Element.centerY
            , Element.padding 5
            ]
    in
    Element.column
        [ Element.width (Element.px 65)
        , Element.height Element.fill
        , Element.spacing 10
        , Element.paddingEach { top = 0, bottom = 0, left = 5, right = 0 }
        ]
        [ Element.Input.button attrs
            { onPress = Just Generate
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "NEW")
            }
        , Element.Input.button attrs
            { onPress = Just PlayMelody
            , label = Element.el [ Element.centerX, Element.centerY ] (Element.text "PLAY")
            }
        , Element.Input.button
            [ Element.alignBottom
            , Element.centerX
            , Element.width (Element.px 40)
            , Element.height (Element.px 40)
            , Element.Border.rounded 20
            , Element.Border.color white
            , Element.Border.width 1
            , Element.Font.color white
            ]
            { onPress = Just ToggleHelp
            , label =
                Element.el
                    [ Element.centerX, Element.centerY ]
                    (Element.text "?")
            }
        ]


renderTopPanel : Model -> Element.Element Msg
renderTopPanel model =
    let
        labels =
            if List.length model.melody == notesPerMelody then
                model.melody |> List.map String.fromInt

            else
                List.range 1 notesPerMelody |> List.map (always "?")
    in
    Element.row
        [ Element.width Element.fill, Element.spacing 20 ]
        [ Element.column
            [ Element.Font.color white
            , Element.height Element.fill
            , Element.Font.family [ Element.Font.typeface "Impact", Element.Font.sansSerif ]
            , Element.Region.heading 1
            ]
            [ Element.text "RANDOM", Element.text "MELODY" ]
        , labels
            |> List.map
                (\label ->
                    Element.el
                        [ Element.Border.width 1
                        , Element.Border.innerGlow blueGlow 1
                        , Element.Font.color blue
                        , Element.Font.glow blueGlow 4
                        , Element.Background.color black
                        , Element.padding 10
                        , Element.Border.rounded 5
                        , Element.width (Element.fillPortion 1)
                        ]
                        (Element.el [ Element.centerX, Element.centerY ] (Element.text label))
                )
            |> Element.row [ Element.width (Element.fillPortion 8), Element.spacing 10, Element.height Element.fill ]
        ]


renderHelpPopup : Element.Element Msg
renderHelpPopup =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color (Element.rgba 0 0 0 0.9)
        , Element.Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 20, bottomRight = 20 }
        , Element.Font.color white
        , Element.htmlAttribute (Html.Attributes.style "z-index" "100")
        , Element.inFront
            (Element.Input.button
                [ Element.Font.color white
                , Element.Font.size 50
                , Element.alignRight
                , Element.alignTop
                , Element.moveLeft 30
                ]
                { onPress = Just ToggleHelp
                , label = Element.el [ Element.rotate (degrees 45) ] (Element.text "+")
                }
            )
        ]
        (Element.textColumn
            [ Element.padding 50, Element.spacing 10 ]
            [ Element.paragraph [ Element.Region.heading 2, Element.Font.size 28, Element.Font.bold, Element.paddingXY 0 5 ] [ Element.text "Music challenge!" ]
            , Element.paragraph [] [ Element.text "• Tap “NEW” to produce a unique 6-note sequence." ]
            , Element.paragraph [] [ Element.text "• Craft a composition using this sequence, keeping the original note order. Feel free to:" ]
            , Element.textColumn [ Element.paddingEach { left = 20, right = 0, top = 0, bottom = 0 }, Element.spacing 5 ]
                [ Element.paragraph [] [ Element.text "• Shift the entire sequence by any number of semitones." ]
                , Element.paragraph [] [ Element.text "• Change any note's octave." ]
                , Element.paragraph [] [ Element.text "• Experiment with different note durations." ]
                , Element.paragraph [] [ Element.text "• If stuck, alter one note by a semitone." ]
                ]
            ]
        )


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
    let
        playedKey =
            case model.synthState of
                Playing melody ->
                    Just (List.length model.melody - List.length melody)

                Stopped ->
                    Nothing

        helpPopup =
            if model.showHelp then
                renderHelpPopup

            else
                Element.none
    in
    Element.column
        [ Element.width (Element.fill |> Element.minimum 800 |> Element.maximum 960)
        , Element.centerX
        , Element.padding 10
        , Element.Background.color darkGray
        , Element.spacing 10
        , Element.Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 20, bottomRight = 20 }
        , Element.inFront helpPopup
        ]
        [ renderTopPanel model
        , Element.row [ Element.width Element.fill ]
            [ renderLeftPanel model
            , renderKeyboard model.keyboard
                (melodyByNote model.melody)
                playedKey
            , renderRightPanel model
            ]
        ]


pushUrl : Browser.Navigation.Key -> List Int -> Int -> Cmd Msg
pushUrl navKey melody shift =
    let
        melodyParams =
            if List.isEmpty melody then
                []

            else
                [ Url.Builder.string "melody" (serializeMelody melody) ]

        shiftParams =
            if shift == 0 then
                []

            else
                [ Url.Builder.int "shift" shift ]
    in
    Browser.Navigation.pushUrl navKey (Url.Builder.relative [] (melodyParams ++ shiftParams))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate (\( shift, melody ) -> Reset shift melody)
                (Random.map2
                    Tuple.pair
                    (Random.int -5 6)
                    (Random.list (notesPerMelody - 1) (Random.int 1 (List.length defaultKeyboard))
                        |> Random.map (\l -> 1 :: l)
                    )
                )
            )

        Nop ->
            ( model, Cmd.none )

        PlayMelody ->
            case model.synthState of
                Stopped ->
                    playMelody model.shift model.melody
                        |> Tuple.mapFirst (\synthState -> { model | synthState = synthState })

                Playing (i :: rest) ->
                    ( { model | synthState = Playing rest }
                    , playNote (i + model.shift)
                    )

                Playing [] ->
                    ( { model | synthState = Stopped }, sendAudioCommand ( "stop", Nothing ) )

        Reset shift melody ->
            let
                ( synthState, cmd ) =
                    playMelody shift melody

                keyboard =
                    shiftKeyboard shift defaultKeyboard
            in
            ( { model | melody = melody, shift = shift, synthState = synthState, keyboard = keyboard }
            , Cmd.batch [ cmd, pushUrl model.navigationKey melody shift ]
            )

        ShiftKeyboard dShift ->
            let
                keyboard =
                    shiftKeyboard dShift model.keyboard

                shift =
                    model.shift + dShift
            in
            ( { model | keyboard = keyboard, shift = shift }, pushUrl model.navigationKey model.melody shift )

        ToggleHelp ->
            ( { model | showHelp = not model.showHelp }, Cmd.none )


playMelody : Int -> List Int -> ( SynthState, Cmd Msg )
playMelody shift melody =
    case melody of
        note :: rest ->
            ( Playing rest, playNote (note + shift) )

        [] ->
            ( Stopped, Cmd.none )


playNote : Int -> Cmd Msg
playNote note =
    Cmd.batch
        [ sendAudioCommand ( "play", Just (noteFrequency note) )
        , Task.perform (always PlayMelody) (Process.sleep 500)
        ]


normalizeMelody : List Int -> ( Maybe Int, List Int )
normalizeMelody melody =
    melody
        |> List.head
        |> Maybe.map
            (\first ->
                if first == 1 then
                    ( Nothing, melody )

                else
                    ( (if first - 1 > 6 then
                        -6 + (first - 7)

                       else
                        first
                            - 1
                      )
                        |> Just
                    , List.map (\i -> modBy (List.length defaultKeyboard) (i - first) + 1) melody
                    )
            )
        |> Maybe.withDefault ( Nothing, melody )


deserialzeMelody : String -> ( Maybe Int, List Int )
deserialzeMelody s =
    let
        tokens =
            String.split "-" s
                |> List.map
                    (String.toInt
                        >> Maybe.andThen
                            (\i ->
                                if i < 1 || i > 12 then
                                    Nothing

                                else
                                    Just i
                            )
                    )

        notes =
            List.filterMap identity tokens
    in
    if List.length tokens == List.length notes && List.length notes == notesPerMelody then
        normalizeMelody notes

    else
        ( Nothing, [] )


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
                    ( melody, shift ) =
                        { url | path = "" }
                            |> Url.Parser.parse
                                (Url.Parser.query
                                    (Url.Parser.Query.map2
                                        Tuple.pair
                                        (Url.Parser.Query.string "melody")
                                        (Url.Parser.Query.int "shift")
                                    )
                                )
                            |> Maybe.map
                                (\( mMelody, mShift ) ->
                                    mMelody
                                        |> Maybe.map deserialzeMelody
                                        |> Maybe.map (\( mMelodyShift, m ) -> ( m, mMelodyShift |> Maybe.withDefault (mShift |> Maybe.withDefault 0) ))
                                        |> Maybe.withDefault ( [], mShift |> Maybe.withDefault 0 )
                                )
                            |> Maybe.withDefault ( [], 0 )
                in
                ( { melody = melody
                  , keyboard = defaultKeyboard |> shiftKeyboard shift
                  , navigationKey = navKey
                  , synthState = Stopped
                  , shift = shift
                  , showHelp = False
                  }
                , if List.isEmpty melody then
                    Cmd.none

                  else
                    pushUrl navKey melody shift
                )
        , view =
            view
                >> (\model ->
                        { title = "Random melody"
                        , body = [ Element.layout [ Element.width Element.fill ] model ]
                        }
                   )
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = always Nop
        , onUrlRequest = always Nop
        }
