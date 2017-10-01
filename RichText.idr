module RichText

import Js.Dom

%default total

data FragMode = Plain | Emph

plain : String -> Html ev
plain = text

emph : String -> Html ev
emph = node "em" (the (List (HtmlAttribute _)) []) . pure . text

export parse : String -> List (Html ev)
parse = finish . foldl step (Plain, [], []) . unpack
  where
    frag : FragMode -> List Char -> Html ev
    frag Plain = plain . pack . reverse
    frag Emph = emph . pack . reverse

    step : (FragMode, List Char, List (Html ev)) -> Char -> (FragMode, List Char, List (Html ev))
    step (mode, s, frags) '*' = (mode', [], frag mode s :: frags)
      where
        mode' = case mode of
          Plain => Emph
          Emph => Plain
    step (mode, s, frags) c = (mode, c :: s, frags)

    finish : (FragMode, List Char, List (Html ev)) -> List (Html ev)
    finish (mode, overhang, frags) = reverse $ if isNil overhang then frags else frag mode overhang :: frags

-- Local Variables:
-- idris-load-packages: ("js")
-- End:
