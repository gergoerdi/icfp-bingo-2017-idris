module Bingo.Web

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall
import Effects
import Effect.Random
import Effect.Random.Shuffle
import Control.ST.LiftEffect


total grid : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
grid Z _ [] = []
grid (S n) m xs = let (ys, yss) = splitAt m xs in ys :: grid n m yss

node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : (a -> List (Html ev)) -> Vect n (Vect m a) -> Html ev
table text xs = node0 "table" [cssClass "table table-bordered"] [node0 "tbody" [] $ toList $ map row xs]
  where
    cell : a -> Html ev
    cell = node0 "td" [] . text

    row : Vect m a -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList . map cell

public export BingoSize : Nat
BingoSize = 4

BingoSheet : Type
BingoSheet = Vect (BingoSize * BingoSize) String

data Command = Shuffle | Print

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const BingoSheet) (const Command) ()

render : () -> BingoSheet -> Html Command
render () spaces = div [stringAttribute "style" "width: 100%; max-width: 600px; margin: auto"]
    [ div [] [map void $ table (pure . text) $ grid BingoSize BingoSize spaces]
    , button [onclick Shuffle, cssClass "btn btn-lg noprint btn-default"]
      "Give me another one"
    , button [onclick Print, cssClass "btn btn-lg noprint btn-primary pull-right"]
      "Print"
    ]

printPage : ST ASync () []
printPage = lift . liftJS_IO $ jscall "window.print()" (JS_IO ())

makeSheet : Vect (BingoSize * BingoSize + _) String -> (seed : Var) -> ST ASync BingoSheet [seed ::: State Integer]
makeSheet items seed = do
    items' <- call $ liftEff seed $ shuffle items
    pure $ take (BingoSize * BingoSize) items'

exec : Vect (BingoSize * BingoSize + _) String -> (dom : Var) -> (seed : Var) -> Command -> ST ASync () [seed ::: State Integer, dom ::: Gui {m =  ASync}]
exec items dom seed cmd = case cmd of
    Shuffle => do
        sheet <- makeSheet items seed
        domPut dom sheet
    Print => do
        printPage

pageLoop : Vect (BingoSize * BingoSize + _) String -> (dom : Var) -> (seed : Var) -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
pageLoop items dom seed = do
    cmd <- getInput dom
    exec items dom seed cmd
    pageLoop items dom seed

page : Vect (BingoSize * BingoSize + _) String -> ST ASync () []
page items = do
    seed <- do
        now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
        new $ cast now

    dom <- do
        sheet <- makeSheet items seed
        initBody [] render () sheet

    pageLoop items dom seed

    delete seed
    clearDom dom

export runPage : Vect (BingoSize * BingoSize + _) String -> JS_IO ()
runPage items = setASync_ $ run (page items)
