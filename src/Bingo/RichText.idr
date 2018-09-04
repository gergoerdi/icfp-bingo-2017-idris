module Bingo.RichText

import Js.Dom

%default total

data FragMode = Plain | Emph | Code

plain : String -> Html ev
plain = text

emph : String -> Html ev
emph = node "em" (the (List (HtmlAttribute _)) []) . pure . text

code : String -> Html ev
code = node "tt" (the (List (HtmlAttribute _)) []) . pure . text

export parse : String -> List (Html ev)
parse = finish . foldl step (Plain, [], []) . unpack
  where
    frag : FragMode -> List Char -> Html ev
    frag Plain = plain . pack . reverse
    frag Emph = emph . pack . reverse
    frag Code = code . pack . reverse

    step : (FragMode, List Char, List (Html ev)) -> Char -> (FragMode, List Char, List (Html ev))
    step (Plain, s, frags) '*' = (Emph, [], frag Plain s :: frags)
    step (Emph, s, frags) '*' = (Plain, [], frag Emph s :: frags)
    step (Plain, s, frags) '`' = (Code, [], frag Plain s :: frags)
    step (Code, s, frags) '`' = (Plain, [], frag Code s :: frags)
    step (mode, s, frags) c = (mode, c :: s, frags)

    finish : (FragMode, List Char, List (Html ev)) -> List (Html ev)
    finish (mode, overhang, frags) = reverse $ if isNil overhang then frags else frag mode overhang :: frags
