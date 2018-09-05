module Main

import Bingo.Web
import Data.Vect
import Js.Dom

freeSpaces : List (Html Void)
freeSpaces =
    [ square
      [ text "FREE "
      , node "strike" (the (List $ HtmlAttribute _) []) . pure . text $ "MONAD"
      , text " SPACE"
      ]
    , square
      [ text "FREE "
      , node "strike" (the (List $ HtmlAttribute _) []) . pure . text $ "ALGEBRA"
      , text " SPACE"
      ]
    ]
  where
    square : List (Html Void) -> Html Void
    square = node0 "td" [stringAttribute "style" "background: #ddd"]

items : List String
items =
    [ "Generalization of monads"
    , "`Nat` as an inductive type"
    , "`fac` as a recursive function"
    , "`Vec` as an indexed type"
    , "Session type that receives a list"
    , "Linear types"
    , "One slide with ≥3 type derivation rules"
    , "A type system has undecidable type checking"
    , "Algebraic effects"
    , "Universe codes"
    , "Ornaments"
    , "Phil Wadler asks a question"
    , "SPJ, from the audience, answers a question"
    , "Insertion sort is proven correct"
    , "Proof relevance"
    , "Quotient types"
    , "JavaScript (non-sarcastically)"
    , "Shirt joke"
    , "Hoare triplets"
    , "DSL"
    , "FRP"
    , "HoTT"
    , "Static tracking of computational cost"
    , "`reverse` as a tail-recursive function w/accumulator"
    , "Pi-calculus"
    ]

main : JS_IO ()
main = runPage (fromList freeSpaces) (fromList items)
