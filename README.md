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
## Make a list and play with it






## Find the last element of a list (L01)
