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

node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : Vect n (Vect m (Html ev)) -> Html ev
table xs = node0 "table" [cssClass "table table-bordered"] [node0 "tbody" [] $ toList $ map row xs]
  where
    row : Vect m (Html ev) -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList

public export BingoSize : Nat
BingoSize = 5

FreeSpace : Html Void
FreeSpace = node0 "td" [stringAttribute "style" "background: #ddd"]
    [ text "FREE "
    , node "strike" (the (List $ HtmlAttribute _) []) . pure . text $ "MONAD"
    , text " SPACE"
    ]

BingoView : Type
BingoView = Bingo BingoSize (Html Void)

toBingo : Vect (BingoSize * BingoSize + _) String -> BingoView
toBingo items = mkBingo BingoSize (FreeSpace, map (node0 "td" [] . parse) $ take (BingoSize * BingoSize - 1) items)

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

exec : Vect (BingoSize * BingoSize + _) String -> (dom : Var) -> (seed : Var) -> Command -> ST ASync () [seed ::: State Integer, dom ::: Gui {m =  ASync}]
exec items dom seed Shuffle = do
    items' <- call $ liftEff seed $ shuffle items
    domPut dom $ toBingo items'
    pure ()
exec items dom seed Print = do
    printPage

pageLoop : Vect (BingoSize * BingoSize + _) String -> (dom : Var) -> (seed : Var) -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
pageLoop items dom seed = do
    cmd <- getInput dom
    exec items dom seed cmd
    pageLoop items dom seed

page : Vect (BingoSize * BingoSize + _) String -> ST ASync () []
page items = do
    dom <- initBody [] render () $ toBingo items
    now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
    seed <- new $ cast now

    exec items dom seed Shuffle
    pageLoop items dom seed

    delete seed
    clearDom dom

export runPage : Vect (BingoSize * BingoSize + _) String -> JS_IO ()
runPage items = setASync_ $ run (page items)
