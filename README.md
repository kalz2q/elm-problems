# elm-problems

Idea is to list simple problems and answers in elm 0.19 without using elm architecture or browser.

I mean to use `elm repl` mainly.

When functions are not written in one line, use editor and include the file like so:

```
import Hoge

Hoge.someFnction x
```
If it is error , edit Hoge.elm and save and try the function again.

`elm repl` is automatically updated if imported file changes.

Anyway lets begin.

## Put strings together
```
> "hello" ++ "world"
"helloworld" : String
```
```
> (++) "hello" "world"
"helloworld" : String
```
## Try math operations
```
> 2 + 3 * 4
14 : number
> (2 + 3) * 4.0
20 : Float
> 9 / 2
4.5 : Float
> 9 // 2
4 : Int
>  3 ^ 2
9 : number
> modBy 3 -7      
2 : Int
> remainderBy 3 -7
-1 : Int
```
## Write add function
```
> add x y = x + y
<function> : number -> number -> number
> add 3 4
7 : number
```
## Write isNegative function
```
> isNegative n = n < 0
<function> : number -> Bool
> isNegative -3
True : Bool
```
## Write `flip` function
```
> flip x y z = x z y
<function> : (a -> b -> c) -> b -> a -> c
> flip (>) 3 4
True : Bool
> (>) 3 4 
False : Bool
> List.map ((>) 3) [1,5,8]
[True,False,False] : List Bool
> List.map (flip (>) 3) [1,5,8]
[False,True,True] : List Bool
```
## Try anonymous (lambda) function
```
> isNegative -3
True : Bool
> (\n -> n < 0) 4
False : Bool
```
## Try if-expression
```
> if True then "hello" else "world"
"hello" : String
> if False then "hello" else #
-- PARSE ERROR
> if False then "hello" else 3
-- TYPE MISMATCH 
```
## Play on lists
```
> [1,2,3]
[1,2,3] : List number
> names = ["Alice", "Bop", "Chuck"]
["Alice","Bop","Chuck"] : List String
> List.isEmpty names
False : Bool
> [1, 2, "three"]
-- TYPE MISMATCH
> List.length names
3 : Int
> String.length "hello"
5 : Int
> List.reverse names
["Chuck","Bop","Alice"] : List String
> numbers = [1,4,3,2]
[1,4,3,2] : List number
> List.sort numbers
[1,2,3,4] : List number
> double n = n * 2
<function> : number -> number
> List.map double numbers
[2,8,6,4] : List number
> 1 :: [2, 3, 4] == 1 :: 2 :: 3 :: 4 :: []
True : Bool
```
## Play on tuples
```
> (3, 4)
(3,4) : ( number, number1 )
> Tuple.pair 3 4
(3,4) : ( number, number1 )
```
```
-- Hoge.elm
goodName name =
    if String.length name <= 20 then
        ( True, "name accepted!" )
    else
        ( False, "name was too long; please limit it to 20 characters" )
```
```
> import Hoge
> Hoge.goodName "tom"
(True,"name accepted!") : ( Bool, String )
```
```
> multipley3rd (x, y, z) = x * y * z
<function> : ( number, number, number ) -> number
> multipley3rd (6, 7, 2)
84 : number
> multiply2d someTuple = let (x, y) = someTuple in x * y
<function> : ( number, number ) -> number
> multiply2d (5, 10)
50 : number
```
## Play on records
```
> point = { x = 3, y = 4 }
{ x = 3, y = 4 }
    : { x : number, y : number1 }
> point.x
3 : number
> bill = { name = "Gates", age = 62 }
{ age = 62, name = "Gates" }
    : { age : number, name : String }
> bill.name
"Gates" : String
.name bill
"Gates" : String
> List.map .name [bill,bill,bill] 
["Gates","Gates","Gates"] : List String
> under70 {age} = age < 70
<function> : { a | age : number } -> Bool
> under70 bill
True : Bool
> under70 { species = "Triceratops", age = 68000000 }
False : Bool
> { bill | name = "Nye" }
{ age = 62, name = "Nye" }
    : { age : number, name : String }
> { bill | age = 22 }
{ age = 22, name = "Gates" }
    : { age : number, name : String }
> bill
{ age = 62, name = "Gates" }
    : { age : number, name : String }
 ```
## Write fib function
```
-- Hoge.elm
module Hoge exposing (..)
fib n =
    case n of
        0 -> 1
        1 -> 1
        _ -> 
          fib (n - 1) + fib (n - 2)
```
```
> Hoge.fib 10
89 : number
```
## pi and e
```
> pi
3.141592653589793 : Float
> e
2.718281828459045 : Float
> "Pi is " ++ String.fromFloat pi ++ " (give or take)"  
"Pi is 3.141592653589793 (give or take)" : String
```
## Play on custum types
```
> type Money = Dollar | EURO | JPY
> Dollar
Dollar : Money
> type Height = Inches Int | Meters Float
> Inches 3
Inches 3 : Height
> Meters
<function> : Float -> Height
> type Location = Nowhere | Somewhere Float Float
> Somewhere
<function> : Float -> Float -> Location
```
## Maybe
```
> Nothing
Nothing : Maybe a
> Just 
<function> : a -> Maybe a
> Just 3
Just 3 : Maybe number
```
## Result
```
> Ok
<function> : value -> Result error value
> Ok "this is ok"
Ok ("this is ok") : Result error String
> Err
<function> : error -> Result error value
> Err "this is error"
Err ("this is error") : Result String value
```
## String functions
```
> String.reverse "this"
"siht" : String
> String.repeat 3 "this"
"thisthisthis" : String
> String.join " is " ["this", "a pen"]
"this is a pen" : String
```
## Pipeline
```
> sanitize input = input |> String.trim |> String.toInt
<function> : String -> Maybe Int
> sanitize " 3 "
Just 3 : Maybe Int
> sanitize2 input = String.toInt (String.trim input)
<function> : String -> Maybe Int
> sanitize2 " 4 "
Just 4 : Maybe Int
```
## Type Inference
```
> toFullName person = person.firstName ++ " " ++ person.lastName
<function> : { a | firstName : String, lastName : String } -> String
> fullName = toFullName { firstName = "Hermann", lastName = "Hesse" }
"Hermann Hesse" : String
```
```
> not True
False : Bool
> round 3.1415
3 : Int
["Alice","Bob"] : List String
> [1.0, 8.6, 42.1]
[1,8.6,42.1] : List Float
> []
[] : List a
> 3::[]
[3] : List number
> String.length
<function> : String -> Int
> String.length " If You Are Not a Conservative at 35 You Have No Brain."
55 : Int
```
## Character literal
```
> 'a'
'a' : Char
> "a"
"a" : String
> ''
-- PARSE ERROR
```
## Shadowing
```
> let x = 3 in x ^3
27 : number
> x = 4
4 : number
> let x = 3 in x ^3
-- SHADOWING
```
## Find the last element of a list (L01)
```
> myLast a = List.head <| List.reverse a
> import Maybe
> Maybe.withDefautl 0 <| myLast [1,2,3,4]
4 : number
```
```
> const a b = a                             
<function> : a -> b -> a
> myLast = List.foldl (const) 0
<function> : List number -> number
> myLast [1,2,3,4]
4 : number
```
## Find the last but one elment of a list (L02)
```
myTail a =
    case a of
        x :: xs ->
            xs

        [] ->
            []

> myButLast a = Maybe.withDefault 0 <| List.head <| myTail <| List.reverse a 
> myButLast [1,2,3,4]
3 : number
```
## Find the K'th element of a list. The first element in the list is number 1 (L03)
Example:
```
elementAt [1,2,3,4] 3
3 : number
```
```
elementAt l a =
    if List.length l <= a then
        0

    else
        let pairs =
               List.indexedMap Tuple.pair l

            selectPair num pair =
                          Tuple.first pair == num - 1
```
##  Find the number of elements of a list (L04)
```
> List.length [1,2,3,4]
4 : Int
> String.length "hello"
5 : Int
```
```
myLength a =
    case a of
        x :: xs ->
            1 + myLength xs

        [] ->
            0

> myLength [1,2,3,4]
4 : number
```
```
> myLength <| String.toList "hello"
5 : number
```
## Reverse a list (L05)
```
> String.reverse "A man, a plan, a canal, panama!" 
"!amanap ,lanac a ,nalp a ,nam A" : String
> List.reverse [1,2,3,4]
[4,3,2,1] : List number
```
```
> myReverse = List.foldl (::) []
<function> : List a -> List a
> myReverse [1,2,3,4]           
[4,3,2,1] : List number
> String.fromList <| myReverse <| String.toList "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A" : String
```
## Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x) (L06)
Example:
```
λ> isPalindrome [1,2,3]
False
λ> isPalindrome "madamimadam"
True
λ> isPalindrome [1,2,4,8,16,8,4,2,1]
True
```
```
> isPalindrome xs = (==) xs  <| List.reverse xs                  
<function> : List a -> Bool
> isPalindromeString xs = (==) xs  <| String.fromList <| List.reverse <| String.toList xs
<function> : String -> Bool
> isPalindrome [1,2,3]
False : Bool
> isPalindromeString "madamimadam"
True : Bool
> isPalindrome [1,2,4,8,16,8,4,2,1]
True : Bool
```
## Flatten a nested list structure (L07)
Example:
```
* (my-flatten '(a (b (c d) e)))
(A B C D E)
```
Unsolvable because Elm list is homogeneous and [1, [2, 3]] is not allowed.  In cases , following will do
```
> List.foldr (++) [] [[1,2],[3,4],[1,1]]
[1,2,3,4,1,1] : List number
```
## Eliminate consecutive duplicates of list elements (L08)
Example:
```
* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
λ> compress "aaaabccaadeeee"
"abcade"
```
```
compress x =
  case x of
    [] -> []
    (y::z::zs) -> if y == z then compress (z::zs) else y::compress (z::zs)
    [_] -> x
```
```
> String.fromList <| Hoge.compress <| String.toList "aaaabccaadeeee" 
"abcade" : String
```
## Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists (L09)

Example:
```
* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
```
```
pack x =
    List.foldr spanfunc [] x

packString x = List.foldr spanfuncString [] x

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
```
```
> Hoge.pack [1,1,1,1,2,3,3,1,1,4,5,5,5,5]
[[1,1,1,1],[2],[3,3],[1,1],[4],[5,5,5,5]]
    : List (List number)

> Hoge.packString ["a", "a", "b", "c","c"]
[["a","a"],["b"],["c","c"]]
    : List (List String)
```
## Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E (L10)
Example:
```
* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
```
```
encode x = 
  List.map encodeHelp (pack x)

encodeHelp x = ((List.length x), Maybe.withDefault 0 (List.head x))
```
```
> Hoge.encode [1,1,1,1,2,3,3,1,1,5,5,5,5,5]
[(4,1),(1,2),(2,3),(2,1),(5,5)]
    : List ( Int, number )
```
## Modified run-length encoding (L11)

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:
```
* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

λ> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
 ```
 Haskell and Elm is homegeneous lists and custome type is needed.
 ```
type ListItem a = Single a | Multiple Int a

encodeModified x = 
  let
      encodeHelper (n,  y) = 
        case n of
          1 -> Single y
          _ -> Multiple n y
  in
    List.map encodeHelper (encode x)

 > Hoge.encodeModified [1,1,1,1,2,3,3,4,5,5,5,5]
[Multiple 4 1,Single 2,Multiple 2 3,Single 4,Multiple 4 5]
```
## Decode a run-length encoded list (L12)
Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
Example:
```
λ> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
```
```
decodeModified : List (ListItem a) -> List a
decodeModified x =
    let
        decodeHelper y =
            case y of
                Single a ->
                    [a]

                Multiple n a ->
                    List.repeat n a
    in
    List.concatMap decodeHelper x
```
```
> decodeModified (encodeModified [1,1,1,1,2,3,3,4,5,5,5,5])     
[1,1,1,1,2,3,3,4,5,5,5,5]
    : List number

```
## Run-length encoding of a list (direct solution) (L13)
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:
```
* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
λ> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
 ```
```
encodeDirect x =
  let
    encodeDirectHelper y z =
      case z of
        [] -> [(1,y)]
        (a,b)::ys ->
          if y == b then (1+a,y)::ys else (1,y)::(a,b)::ys

  in
    List.foldr encodeDirectHelper [] x 

> Hoge.encodeDirect [1,1,1,1,2,3,3,4,5,5,5,5]
[(4,1),(1,2),(2,3),(1,4),(4,5)]
    : List ( number, number1 )
```
## Duplicate the elements of a list (L14)
Example:
```
* (dupli '(a b c c d))
(A A B B C C C C D D)
λ> dupli [1, 2, 3]
[1,1,2,2,3,3]
```
```
> List.concatMap (List.repeat 2) [1, 2, 3, 3, 4]
[1,1,2,2,3,3,3,3,4,4]
    : List number
```
```
dupli x =
  case x of
    [] -> []
    (y::ys) -> y::y::(dupli ys)
```
```
> Hoge.dupli [1,2,3,3,4]
[1,1,2,2,3,3,3,3,4,4]
    : List number
```
```
> List.concatMap (\n -> [n, n]) [1,2,3,3,4]
[1,1,2,2,3,3,3,3,4,4]
    : List number
```
## Replicate the elements of a list a given number of times (L15)

Example:
```
* (repli '(a b c) 3)
(A A A B B B C C C)
Example in Haskell:

λ> repli "abc" 3
"aaabbbccc"
```
```
> List.concatMap (List.repeat 3) [1,2,3]
[1,1,1,2,2,2,3,3,3]
    : List number
```
## Drop every N'th element from a list (L16)
Example:
```
* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
λ> dropEvery "abcdefghik" 3
"abdeghk"
```
```
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
```
```
> dropEvery [1,2,3,4,5,6,7,8,9,10] 3
[1,2,4,5,7,8,10]
    : List number
```
## Split a list into two parts; the length of the first part is given (L17)
Do not use any predefined predicates.
Example:
```
* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
λ> split "abcdefghik" 3
("abc", "defghik")
```
```
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
```
```
> Hoge.split [1,2,3,4,5,6,7] 2
[[1,2],[3,4,5,6,7]]
    : List (List number)
> Hoge.split [1,2,3,4,5,6,7] 5
[[1,2,3,4,5],[6,7]]
    : List (List number)
> Hoge.split [1,2] 5
[[1,2],[]] : List (List number)
```
## Extract a slice from a list (L18)
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

Example:
```
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)
λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
```
```
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
```
```
> List.range 0 10
[0,1,2,3,4,5,6,7,8,9,10]
    : List Int
> slice (List.range 0 10)  2 5
[2,3,4] : List Int
```



