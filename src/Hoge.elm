-- Hoge.elm
module Hoge exposing (..)
fib n =
    case n of
        0 -> 1
        1 -> 1
        _ -> 
          fib (n - 1) + fib (n - 2)


goodName name =
    if String.length name <= 20 then
        ( True, "name accepted!" )

    else
        ( False, "name was too long; please limit it to 20 characters" )
