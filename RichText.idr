module RichText

import Js.Dom
import Lightyear
import Lightyear.Char
import Lightyear.Strings

data Frag = Plain String | Emph String

Show Frag where
    show (Plain s) = "|" ++ s ++ "|"
    show (Emph s) = "*" ++ s ++ "*"

char : Parser Char
char = satisfy (/= '*')

plain : Parser String
plain = pack <$> some char

emph : Parser String
emph = char '*' *> plain <* char '*'

frag : Parser Frag
frag = Plain <$> plain <|>| Emph <$> emph

export parse : String -> Either String (List (Html ev))
parse = map (html <$>) . Lightyear.Strings.parse (many frag)
  where
    html (Plain s) = text s
    html (Emph s) = node "em" (the (List (HtmlAttribute _)) []) [text s]

-- Local Variables:
-- idris-load-packages: ("lightyear" "js")
-- End:
