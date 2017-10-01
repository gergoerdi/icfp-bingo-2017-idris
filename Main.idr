module Main

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall
import Shuffle
import Effects
import Effect.Random

data Command = Shuffle -- | Print

total grid : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
grid Z _ [] = []
grid (S n) m xs = let (ys, yss) = splitAt m xs in ys :: grid n m yss

node0 : String -> List (HtmlAttribute Void) -> List (Html Void) -> Html Void
node0 = node

table : Vect n (Vect m String) -> Html Void
table xs = node0 "table" [cssClass "table table-bordered"] [node0 "tbody" [] $ toList $ map row xs]
  where
    cell : String -> Html Void
    cell = node0 "td" [] . (::[]) . text

    row : Vect m String -> Html Void
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
    [ div [] [map void $ table $ grid BingoSize BingoSize spaces]
    , button [onclick Shuffle, cssClass "btn btn-default btn-lg noprint"]
      "Give me another one"
    ]

pageLoop : (Dom m, Monad m) => (d : Var) -> (seed : Var) -> ST m () [seed ::: State Integer, d ::: Gui {m}]
pageLoop d seed = do
    x <- getInput d
    -- case x of
    --   Shuffle => do
    --     s <- read seed
    --     items' <- lift $ runInit [s] $ shuffle items
    --     domPut d $ take (BingoSize * BingoSize) items'
    --     write seed (s + 1)
    s <- read seed
    items' <- lift $ runInit [s] $ shuffle items
    domPut d $ take (BingoSize * BingoSize) items'
    write seed (s + 1)
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
