# elm-problems

idea is to list simple problems and answers in elm 0.19 without using elm architecture or browser.

everything is function in elm so let's write functions in Hoge.elm and start 'elm repl' elm and import the file

```
import Hoge
```
and try the function like so

```
Hoge.someFnction x
```
if it is error , edit Hoge.elm and save and try the function again.

elm repl is automatically updated if imported file changes.

the problems are from various sources like 99 lisp problems, 4clojure, etc.  
for reference , Lnn is 99 lisp problems 4nnn is 4clojure.

anyway lets begin.

## Put strings together
solution
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
> modBy 2 9
1 : Int
>  3 ^ 2
9 : number
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
## Play on custum types
```
> type Money = Dollar | EURO | JPY
> Dollar
Dollar : Money
```

## Find the last element of a list (L01)
