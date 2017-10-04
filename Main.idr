module Main

import Shuffle
import RichText
import Bingo

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall
import Effects
import Effect.Random
import EffectToST


data Command = Shuffle | Print

node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : (a -> List (Html ev)) -> Vect n (Vect m a) -> Html ev
table text xs = node0 "table" [cssClass "table table-bordered"] [node0 "tbody" [] $ toList $ map row xs]
  where
    cell : a-> Html ev
    cell = node0 "td" [] . text

    row : Vect m a -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList . map cell

items : Vect 17 String
items =
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
    ]

Gui : (Dom m) => Nat -> Type
Gui {m} n = DomRef {m} () (const $ Bingo n String) (const Command) ()

render : () -> Bingo n String -> Html Command
render () spaces = div [stringAttribute "style" "width: 100%; max-width: 600px; margin: auto"]
    [ div [] [map void $ table parse $ grid spaces]
    , button [onclick Shuffle, cssClass "btn btn-lg noprint btn-default"]
      "Give me another one"
    , button [onclick Print, cssClass "btn btn-lg noprint btn-primary pull-right"]
      "Print"
    ]

printPage : ST ASync () []
printPage = lift . liftJS_IO $ jscall "window.print()" (JS_IO ())

exec : (dom : Var) -> (seed : Var) -> Command -> ST ASync () [seed ::: State Integer, dom ::: Gui {m =  ASync} 4]
exec dom seed Shuffle = do
    items' <- call $ liftEff seed $ shuffle items
    domPut dom $ take 16 items'
exec dom seed Print = do
    printPage

pageLoop : (dom : Var) -> (seed : Var) -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync} 4]
pageLoop dom seed = do
    cmd <- getInput dom
    exec dom seed cmd
    pageLoop dom seed

page : ST ASync () []
page = do
    dom <- initBody [] (render {n = 4}) () $ pure ""
    now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
    seed <- new $ cast now

    exec dom seed Shuffle
    pageLoop dom seed

    delete seed
    clearDom dom

main : JS_IO ()
main = setASync_ $ run page

-- Local Variables:
-- idris-load-packages: ("contrib" "js" "effects")
-- End:
