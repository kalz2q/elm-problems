module Fuga exposing (Suit(..), zero100)

import Random


zero100 : Random.Generator Int
zero100 =
    Random.int 0 100


type Suit
    = Hearts
    | Diamonds
    | Spades
    | Clubs


numToSuit : Int -> Suit
numToSuit num =
    case num of
        0 ->
            Hearts

        1 ->
            Diamonds

        2 ->
            Spades

        _ ->
            Clubs
          

suitGenerator : Generator Suit
suitGenerator =
  Random.map numToSuit (Random.int 0 3)