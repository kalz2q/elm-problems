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


```

