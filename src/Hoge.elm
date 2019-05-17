-- Hoge.elm


module Hoge exposing (..)

import Debug
import Dict exposing (Dict, empty, update)





compress x =
  case x of
    [] -> []
    (y::z::zs) -> if y == z then compress (z::zs) else y::compress (z::zs)
    [_] -> x

addToMaybe x m =
  case m of
    Nothing -> Just [x]
    Just xs -> Just (xs ++ [x])


groupBy : (a -> comparable)-> List a -> Dict comparable (List a)
groupBy fun =
  let 
    add2Maybe x m =
      case m of
        Nothing -> Just [x]
        Just xs -> Just (xs ++ [x]) 

    -- add2Maybe x = -- alternative implementation
    --   Just << (flip (++)) [x] << Maybe.withDefault [] 

    foldF e = 
      update (fun e) (add2Maybe e)
  in 
    List.foldl foldF empty

-- > groupBy floor [1.3, 2.1, 2.4, 1.2, 5.0] 
-- Dict.fromList [(1,[1.3,1.2]),(2,[2.1,2.4]),(5,[5])]


myLength a =
    case a of
        x :: xs ->
            1 + myLength xs

        [] ->
            0


elementAt l a =
    if List.length l <= a then
        0

    else
        let
            pairs =
                List.indexedMap Tuple.pair l

            selectPair num pair =
                Tuple.first pair == num - 1
        in
        Maybe.withDefault 0 <| List.head <| List.map Tuple.second <| List.filter (selectPair a) pairs


myTail a =
    case a of
        x :: xs ->
            xs

        [] ->
            []


myLast a =
    List.head <| List.reverse a


toFullName person =
    person.firstName ++ " " ++ person.lastName


fib n =
    case n of
        0 ->
            1

        1 ->
            1

        _ ->
            fib (n - 1) + fib (n - 2)


goodName name =
    if String.length name <= 20 then
        ( True, "name accepted!" )

    else
        ( False, "name was too long; please limit it to 20 characters" )
