module Brick.Span where

import Brick
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

data Span = Span {spanOffset :: Int, spanLength :: Int}
  deriving (Eq, Ord, Show)

getSpan :: Text -> Span -> Text
getSpan t (Span offset len) = T.take len . T.drop offset $ t

splitSpan :: Text -> Span -> (Text, Text, Text)
splitSpan t (Span offset len) = (l, m, r)
  where
    (l, m') = T.splitAt offset t
    (m, r) = T.splitAt len m'

-- Good for one line
txtAttrSpan :: Span -> AttrName -> Text -> Widget n
txtAttrSpan s attr t = hBox [txt l, withAttr attr (txt m), txt r]
  where
    (l, m, r) = splitSpan t s

-- Good for one line. Work backwards so you don't need to adjust spans after
txtAttrSpans :: V.Attr -> [(Span, V.Attr)] -> Text -> V.Image
txtAttrSpans txtAttr spans = go (reverse spans)
  where
    -- Last span first
    go [] t = V.text' txtAttr t
    go ((s, attr) : ss) t = V.horizCat [go ss l, V.text' attr m, V.text' txtAttr r]
      where
        (l, m, r) = splitSpan t s

intersection :: Span -> Span -> Maybe Span
intersection (Span o1 l1) (Span o2 l2)
  | r > l = Just $ Span l (r - l)
  | otherwise = Nothing
  where
    l = max o1 o2
    r = min (o1 + l1) (o2 + l2)

shiftSpan :: Int -> Span -> Span
shiftSpan left (Span o1 l1) = Span (o1 - left) l1
