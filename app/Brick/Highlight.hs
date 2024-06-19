module Brick.Highlight where

import Brick
import Brick.Span
import Control.DeepSeq
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Text.Wrap

txtWrapHighlight :: [(Span, AttrName)] -> WrapSettings -> T.Text -> Widget n
txtWrapHighlight highlightSpans settings s =
  Widget Greedy Fixed $ do
    c <- getContext
    let theLines = fixEmpty <$> wrapTextToLines settings (c ^. availWidthL) s

        theSpans = fixUpsSpans (map (\(s, an) -> (s, attrMapLookup an (c ^. ctxAttrMapL))) $ sortOn fst highlightSpans) theLines

        fixEmpty l
          | T.null l = " "
          | otherwise = l
    case zip (theSpans ++ repeat []) (force theLines) of
      [] -> return emptyResult
      multiple ->
        let maxLength = maximum $ textWidth . snd <$> multiple
            padding = V.charFill (c ^. attrL) ' ' (c ^. availWidthL - maxLength) (length lineImgs)
            lineImgs = lineImg <$> multiple
            lineImg (spans, lStr) =
              let rPad = V.text' (c ^. attrL) $ T.replicate (maxLength - textWidth lStr) " "
               in V.horizCat [txtAttrSpans (c ^. attrL) spans lStr, rPad]
         in return $ emptyResult & imageL .~ (V.horizCat [V.vertCat lineImgs, padding])

fixUpsSpans :: (Show a) => [(Span, a)] -> [T.Text] -> [[(Span, a)]]
fixUpsSpans ss ts = go ss ts (0, 0)
  where
    go [] _ _ = []
    go _ [] _ = []
    go spans' (t : ts) (cur, curOffset) = zip (map toLocal ss) xs : go (zip ss' xsRest) ts (cur + T.length t, newOffset)
      where
        newOffset = if T.all isSpace t then curOffset else curOffset + 1

        (spans, xs) = unzip spans'
        xsRest = drop (length ss) xs

        (ss, ss') = span (\s -> isJust $ intersection s lineSpan) spans
        lineSpan = Span (cur + curOffset) $ T.length t

        toLocal = shiftSpan (cur + curOffset)
