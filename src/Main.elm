module Main exposing (main, update, view)

import Array
import Browser
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (selected, size, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import Music
import String
import Util


type Msg
    = SetTonic String
    | SetScaleIndex String
    | SetInputChordsString String


type alias Model =
    { tonic : Int, scaleIndex : Int, inputChordsString : String }


initialModel =
    { tonic = 0, scaleIndex = 0, inputChordsString = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTonic tonic ->
            { model | tonic = Util.indexDefault tonic model.tonic }

        SetScaleIndex scaleIndex ->
            { model | scaleIndex = Util.indexDefault scaleIndex model.scaleIndex }

        SetInputChordsString inputChordsString ->
            { model | inputChordsString = inputChordsString }


noteOptions selectedIndex =
    Array.indexedMap
        (\index noteName ->
            option
                ([ value (String.fromInt index) ]
                    ++ (if index == selectedIndex then
                            [ selected True ]

                        else
                            []
                       )
                )
                [ text noteName ]
        )
        Music.noteNames
        |> Array.toList


scaleOptions selectedIndex =
    Array.indexedMap
        (\index ( scaleName, _ ) ->
            option
                ([ value (String.fromInt index) ]
                    ++ (if index == selectedIndex then
                            [ selected True ]

                        else
                            []
                       )
                )
                [ text scaleName ]
        )
        Music.scaleTypes
        |> Array.toList


inputChords model =
    Util.nonblankWords model.inputChordsString


outputChords : Model -> List String
outputChords model =
    let
        scale =
            Music.Scale
                (Music.scaleIndexIntervals model.scaleIndex)
                model.tonic
    in
    List.map
        (\inputChord ->
            Music.convertChord scale inputChord
                |> Maybe.withDefault "?"
        )
        (inputChords model)


view : Model -> Html.Html Msg
view model =
    div []
        [ div []
            [ text "Key: "
            , select [ onInput SetTonic ] (noteOptions model.tonic)
            , select [ onInput SetScaleIndex ] (scaleOptions model.scaleIndex)
            ]
        , div []
            [ text "Chords: "
            , input
                [ value model.inputChordsString
                , size 50
                , onInput SetInputChordsString
                ]
                []
            ]
        , div []
            [ text (String.join " " (outputChords model))
            ]
        ]


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
