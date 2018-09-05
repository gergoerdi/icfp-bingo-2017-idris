module Bingo.Web

import Bingo.RichText
import Bingo.Bingo

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall
import Effects
import Effect.Random
import Effect.Random.Shuffle
import Control.ST.LiftEffect


data Command = Shuffle | Print

export node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : Vect n (Vect m (Html ev)) -> Html ev
table xs = node0 "table" [cssClass "table table-bordered"] [node0 "tbody" [] $ toList $ map row xs]
  where
    row : Vect m (Html ev) -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList

public export BingoSize : Nat
BingoSize = 5

BingoView : Type
BingoView = Bingo BingoSize (Html Void)

toBingo : Html Void -> Vect (BingoSize * BingoSize + _) String -> BingoView
toBingo freeSpace items = mkBingo BingoSize (freeSpace, map (node0 "td" [] . parse) $ take (BingoSize * BingoSize - 1) items)

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const BingoView) (const Command) ()

render : () -> BingoView -> Html Command
render () spaces = div [stringAttribute "style" "width: 100%; max-width: 600px; margin: auto"]
    [ div [] [map void $ table $ grid spaces]
    , button [onclick Shuffle, cssClass "btn btn-lg noprint btn-default"]
      "Give me another one"
    , button [onclick Print, cssClass "btn btn-lg noprint btn-primary pull-right"]
      "Print"
    ]

printPage : ST ASync () []
printPage = lift . liftJS_IO $ jscall "window.print()" (JS_IO ())

exec
    : Vect (S _) (Html Void)
    -> Vect (BingoSize * BingoSize + _) String
    -> (dom : Var)
    -> (seed : Var)
    -> Command
    -> ST ASync () [seed ::: State Integer, dom ::: Gui {m =  ASync}]
exec freeSpaces items dom seed Shuffle = do
    items' <- call $ liftEff seed $ shuffle items
    freeSpace <- call $ liftEff seed $ rndSelect' freeSpaces
    domPut dom $ toBingo freeSpace items'
    pure ()
exec freeSpaces items dom seed Print = do
    printPage

pageLoop
    : Vect (S _) (Html Void)
    -> Vect (BingoSize * BingoSize + _) String
    -> (dom : Var)
    -> (seed : Var)
    -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
pageLoop freeSpaces items dom seed = do
    cmd <- getInput dom
    exec freeSpaces items dom seed cmd
    pageLoop freeSpaces items dom seed

page : Vect (S _) (Html Void) -> Vect (BingoSize * BingoSize + _) String -> ST ASync () []
page freeSpaces items = do
    dom <- initBody [] render () $ toBingo (text "") (replicate (BingoSize * BingoSize) "")
    now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
    seed <- new $ cast now

    exec freeSpaces items dom seed Shuffle
    pageLoop freeSpaces items dom seed

    delete seed
    clearDom dom

export runPage : Vect (S _) (Html Void) -> Vect (BingoSize * BingoSize + _) String -> JS_IO ()
runPage freeSpaces items = setASync_ $ run $ page freeSpaces items
