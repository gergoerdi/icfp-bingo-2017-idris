module Main

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall
import Shuffle
import Effects
import Effect.Random


data Command = Shuffle | Print

total grid : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
grid Z _ [] = []
grid (S n) m xs = let (ys, yss) = splitAt m xs in ys :: grid n m yss

node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : (a -> Html ev) -> Vect n (Vect m a) -> Html ev
table text xs = node0 "table" [cssClass "table table-bordered"] [node0 "tbody" [] $ toList $ map row xs]
  where
    cell : a-> Html ev
    cell = node0 "td" [] . (::[]) . text

    row : Vect m a -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList . map cell

items : Vect 17 String
items =
    [ "Generalization of monads"
    , "<tt>Nat</tt> as an inductive type"
    , "<tt>fac</tt> as a recursive function"
    , "<tt>Vec</tt> as a indexed type"
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
    ]

BingoSize : Nat
BingoSize = 4

BingoView : Type
BingoView = Vect (BingoSize * BingoSize) String

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const BingoView) (const Command) ()

render : () -> BingoView -> Html Command
render () spaces = div [stringAttribute "style" "width: 100%; max-width: 600px; margin: auto"]
    [ div [] [map void $ table text $ grid BingoSize BingoSize spaces]
    , button [onclick Shuffle, cssClass "btn btn-default btn-lg noprint"]
      "Give me another one"
    , button [onclick Print, cssClass "btn btn-primary btn-lg pull-right noprint"]
      "Print"
    ]

exec : (Dom m, Monad m) => (d : Var) -> (seed : Var) -> Command -> ST m () [seed ::: State Integer, d ::: Gui {m}]
exec d seed Shuffle = do
    s <- read seed
    items' <- lift $ runInit [s] $ shuffle items
    domPut d $ take (BingoSize * BingoSize) items'
    write seed (s + 1)
exec d seed Print = do
    pure ()

pageLoop : (Dom m, Monad m) => (d : Var) -> (seed : Var) -> ST m () [seed ::: State Integer, d ::: Gui {m}]
pageLoop d seed = do
    x <- getInput d
    exec d seed x
    pageLoop d seed

page : (Dom m, Monad m) => ST m () []
page = do
    dom <- initBody [] render () $ take (BingoSize * BingoSize) items
    seed <- new 0
    pageLoop dom seed
    delete seed
    clearDom dom

main : JS_IO ()
main = setASync_ $ run page

-- Local Variables:
-- idris-load-packages: ("contrib" "js" "effects")
-- End:
