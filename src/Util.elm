module Util exposing (arraySomeIndex, indexDefault, nonblankWords, stringEqualFold)

import Array
import Maybe
import String


arraySomeIndex : (a -> Bool) -> Array.Array a -> Maybe Int
arraySomeIndex predicate ary =
    Array.toIndexedList ary
        |> List.filter (\( _, elem ) -> predicate elem)
        |> List.head
        |> Maybe.map (\( index, elem ) -> index)


stringEqualFold a b =
    String.toLower a == String.toLower b


indexDefault indexString default =
    String.toInt indexString |> Maybe.withDefault default


nonblankWords str =
    String.words str |> List.filter (\s -> not (String.isEmpty s))
