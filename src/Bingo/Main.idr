module Main

import Bingo.Web
import Data.Vect

main : JS_IO ()
main = runPage $
    [ "Generalization of monads"
    , "`Nat` as an inductive type"
    , "`fac` as a recursive function"
    , "`Vec` as a indexed type"
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
