-- Hoge.elm


module Hoge exposing (..)

import Debug
import Random
import Dict exposing (Dict, empty, update)

randomSelect x n = 
  if n <= 0 then [] else
    let
     i = Random.int 1 (List.length x)
    in 
     (takeAt x i) ++ (randomSelect (removeAt x i) (n - 1))

takeAt x n =
  take (drop x n) 1


insertAt x xs n =
  (take xs n) ++ (x::(drop xs n))


removeAt x n = (++) (take x n) (drop x (n+1))


rotate x n =
  let 
    i =  if n >= 0 then n else  (List.length x) + n
  in 
    (++) (drop x i)  (take x i)



slice x m n =
  take  (drop x m) (n - m)


drop x n =
   case x of
     [] -> []
     y::ys ->
       if n > 0 then
         drop ys (n-1)
       else 
         (y::ys)



take x n =
   case x of
     [] -> []
     y::ys ->
       if n > 0 then
         y :: take ys (n-1)
       else 
         []


split x n =
    case x of
        [] ->
            [ [], [] ]

        y :: ys ->
            let
                zs =
                    Maybe.withDefault [] (List.head (split ys (n - 1)))

                zx =
                    Maybe.withDefault [] (List.head (Maybe.withDefault [] (List.tail (split ys (n - 1)))))
            in
            if n > 0 then
                [ y :: zs, zx ]

            else
                [ [], y :: ys ]


dropEvery x n =
    let
        dropEveryHelper y m i =
            case y of
                [] ->
                    []

                z :: zx ->
                    (if remainderBy m i == 0 then
                        []

                     else
                        [ z ]
                    )
                        ++ dropEveryHelper zx m (i + 1)
    in
    dropEveryHelper x n 1


dupli x =
    case x of
        [] ->
            []

        y :: ys ->
            y :: y :: dupli ys


encodeDirect x =
    let
        encodeDirectHelper y z =
            case z of
                [] ->
                    [ ( 1, y ) ]

                ( a, b ) :: ys ->
                    if y == b then
                        ( 1 + a, y ) :: ys

                    else
                        ( 1, y ) :: ( a, b ) :: ys
    in
    List.foldr encodeDirectHelper [] x


decodeModified : List (ListItem a) -> List a
decodeModified x =
    let
        decodeHelper y =
            case y of
                Single a ->
                    [ a ]

                Multiple n a ->
                    List.repeat n a
    in
    List.concatMap decodeHelper x


type ListItem a
    = Single a
    | Multiple Int a


encodeModified x =
    let
        encodeHelper ( n, y ) =
            case n of
                1 ->
                    Single y

                _ ->
                    Multiple n y
    in
    List.map encodeHelper (encode x)


encode x =
    List.map encodeHelp (pack x)


encodeHelp x =
    ( List.length x, Maybe.withDefault 0 (List.head x) )


pack x =
    List.foldr spanfunc [] x


packString x =
    List.foldr spanfuncString [] x


spanfunc x y =
    case y of
        [] ->
            [ [ x ] ]

        [ z :: zs ] ->
            if x == Maybe.withDefault 99 (List.head (Maybe.withDefault [] (List.head y))) then
                [ z :: z :: zs ]

            else
                [ [ x ], z :: zs ]

        (a :: b) :: c :: d ->
            if x == Maybe.withDefault 99 (List.head (Maybe.withDefault [] (List.head y))) then
                (a :: a :: b) :: c :: d

            else
                [ x ] :: (a :: b) :: c :: d

        [] :: a ->
            a


spanfuncString x y =
    case y of
        [] ->
            [ [ x ] ]

        [ z :: zs ] ->
            if x == Maybe.withDefault "" (List.head (Maybe.withDefault [] (List.head y))) then
                [ z :: z :: zs ]

            else
                [ [ x ], z :: zs ]

        (a :: b) :: c :: d ->
            if x == Maybe.withDefault "" (List.head (Maybe.withDefault [] (List.head y))) then
                (a :: a :: b) :: c :: d

            else
                [ x ] :: (a :: b) :: c :: d

        [] :: a ->
            a


compress x =
    case x of
        [] ->
            []

        y :: z :: zs ->
            if y == z then
                compress (z :: zs)

            else
                y :: compress (z :: zs)

        [ _ ] ->
            x


addToMaybe x m =
    case m of
        Nothing ->
            Just [ x ]

        Just xs ->
            Just (xs ++ [ x ])


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy fun =
    let
        add2Maybe x m =
            case m of
                Nothing ->
                    Just [ x ]

                Just xs ->
                    Just (xs ++ [ x ])

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
