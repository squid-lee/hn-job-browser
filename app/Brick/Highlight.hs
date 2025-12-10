module Brick.Highlight (txtWrapHighlight, fixUpsSpans, lineStarts) where

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
    let wrappedLines = wrapTextToLines settings (c ^. availWidthL) s
        theLines = fixEmpty <$> wrappedLines

        -- Use original text to compute exact line start positions
        theSpans = fixUpsSpans (map (\(s, an) -> (s, attrMapLookup an (c ^. ctxAttrMapL))) $ sortOn fst highlightSpans) s wrappedLines

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
         in return $ emptyResult & imageL .~ V.horizCat [V.vertCat lineImgs, padding]

-- Compute where each wrapped line starts in the original text
lineStarts :: T.Text -> [T.Text] -> [Int]
lineStarts original = go 0
  where
    go _ [] = []
    go pos (line : rest)
      | T.null line = pos : go pos rest -- empty line, position unchanged
      | otherwise = case T.breakOn line (T.drop pos original) of
          (before, _) -> (pos + T.length before) : go (pos + T.length before + T.length line) rest

fixUpsSpans :: (Show a) => [(Span, a)] -> T.Text -> [T.Text] -> [[(Span, a)]]
fixUpsSpans ss original ts = go ss (zip (lineStarts original ts) ts)
  where
    go [] _ = []
    go _ [] = []
    go spans' ((lineStart, t) : rest) = localSpans : go nextSpans rest
      where
        lineEnd = lineStart + T.length t
        lineSpan = Span lineStart (T.length t)

        (spans, xs) = unzip spans'
        (intersecting, notYet) = span (\s -> isJust $ intersection s lineSpan) spans
        (xsInt, xsNotYet) = splitAt (length intersecting) xs

        toLocalClipped (Span o l) =
          let o' = max 0 (o - lineStart)
              end = min (o + l - lineStart) (T.length t)
           in Span o' (max 0 (end - o'))

        localSpans = zip (map toLocalClipped intersecting) xsInt

        extendsPast (Span o l) = o + l > lineEnd
        continuing = [(s, x) | (s, x) <- zip intersecting xsInt, extendsPast s]
        nextSpans = continuing ++ zip notYet xsNotYet
