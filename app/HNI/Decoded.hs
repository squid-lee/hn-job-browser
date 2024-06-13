module HNI.Decoded where

import Data.Text (Text)
import qualified Data.Text as T
import Text.HTMLEntity

newtype Decoded = Decoded {payload :: Text}

decode :: Text -> Decoded
decode =
  Decoded
    . decode' -- replacing [("&quot;", "'"), ("&amp;", "&"), ("&#x2F;", "/"), ("&#x27;", "'")]
    . removeTags
    . replacing [("<p>", "\n\n")]
  where
    removeTags s = case T.uncons s of
      Nothing -> s
      Just ('<', s) -> removeTags $ T.drop 1 . T.dropWhile (/= '>') $ s
      Just (x, s) -> T.cons x $ removeTags s

replacing :: [(Text, Text)] -> Text -> Text
replacing tblTop s = go tblTop s
  where
    go [] s = case T.uncons s of
      Nothing -> s
      Just (x, xs) -> T.cons x $ go tblTop xs
    go ((pref, replace) : tbl) s =
      case T.stripPrefix pref s of
        Nothing -> go tbl s
        Just s' -> T.append replace $ go tblTop s'
