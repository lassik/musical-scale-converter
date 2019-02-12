module Music exposing (Scale(..), convertChord, lowerRomanNumerals, noteNames, scaleIndexIntervals, scaleTypes, upperRomanNumerals)

import Array
import Debug
import Dict
import Maybe
import String
import Util


type Scale
    = Scale (List Int) Int


scaleTypes =
    Array.fromList
        [ ( "Major", [ 2, 2, 1, 2, 2, 2 ] )
        , ( "Minor", [ 2, 1, 2, 2, 1, 2 ] )
        ]


noteNames =
    Array.fromList
        [ "C"
        , "Db"
        , "D"
        , "Eb"
        , "E"
        , "F"
        , "F#"
        , "G"
        , "Ab"
        , "A"
        , "Bb"
        , "B"
        ]


upperRomanNumerals =
    Array.fromList
        [ "I"
        , "II"
        , "III"
        , "IV"
        , "V"
        , "VI"
        , "VII"
        ]


lowerRomanNumerals =
    Array.map String.toLower upperRomanNumerals


scaleIndexIntervals : Int -> List Int
scaleIndexIntervals scaleIndex =
    let
        ( _, intervals ) =
            Array.get scaleIndex scaleTypes |> Maybe.withDefault ( "", [] )
    in
    intervals


buildLookupX semitone scaleDegree oldIntervals oldLookup =
    let
        lookup =
            Dict.insert (modBy 12 semitone) scaleDegree oldLookup
    in
    case oldIntervals of
        [] ->
            lookup

        interval :: intervals ->
            buildLookupX (semitone + interval) (scaleDegree + 1) intervals lookup


buildLookup : Scale -> Dict.Dict Int Int
buildLookup (Scale intervals tonic) =
    buildLookupX tonic 0 intervals Dict.empty


scaleDegreeByNoteName : Scale -> String -> Maybe Int
scaleDegreeByNoteName scale noteName =
    Util.arraySomeIndex (Util.stringEqualFold noteName) noteNames
        |> Maybe.andThen (\semitone -> Dict.get semitone (buildLookup scale))


convertChord : Scale -> String -> Maybe String
convertChord scale inputChord =
    scaleDegreeByNoteName scale inputChord
        |> Maybe.andThen (\scaleDegree -> Array.get scaleDegree upperRomanNumerals)
