{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}

module HNI.Salience where

import Brick.Span hiding (getSpan)
import Data.Array as A
import Data.List
import Data.Text (Text)
import GHC.Generics
import HNI.Decoded
import HNI.Post
import Text.Regex.TDFA

data Salient
  = Location Text Span
  | Salary Text Span
  | Remoteness Text Span
  | Tech Text Span
  | URL Text Span
  | Email Text Span
  | Purpose Text Span
  deriving (Generic, Eq, Ord, Show)

salients :: Post Decoded -> [Salient]
salients p =
  removeSubsumed
    . concatMap (\(mkTag, regexp) -> map (uncurry mkTag . toSpan) $ matchAllText regexp (payload (text p)))
    . concat
    $ [ location,
        remoteness,
        salary,
        url,
        email,
        purpose
      ]
  where
    toSpan :: MatchText Text -> (Text, Span)
    toSpan m = (match, Span offset length)
      where
        (match, (offset, length)) = m A.! 0

    i :: String -> Regex
    i = makeRegexOpts (defaultCompOpt {caseSensitive = False}) defaultExecOpt

    r :: String -> Regex
    r = makeRegexOpts (defaultCompOpt {caseSensitive = True}) defaultExecOpt

    curr = "[£$€]|USD|EUR|GBP"

    -- Range: $150k - $200k, 100-150k, $200,000 - $245,000, 150-230k+
    -- Open-ended: 200k+, $200k+
    -- Single: £80k 80k€ (require k suffix to avoid matching $50mil funding)
    salary =
      [ (Salary, i $ "(" <> curr <> ")[[:digit:],]+k[ ]*[-–—][ ]*(" <> curr <> ")?[[:digit:],]+k?\\+?"),
        (Salary, i "[[:digit:]]{2,3}k?[ ]*[-–—][ ]*[[:digit:]]{2,3}k\\+?"),
        (Salary, i $ "(" <> curr <> ")?[[:digit:]]{2,3}k\\+"),
        (Salary, i $ "(" <> curr <> ")[[:digit:],]+k"),
        (Salary, i "[[:digit:]]{2,}[k€£$]+"),
        (Salary, i "equity")
      ]

    url = [(URL, i "https?://[[:alnum:]]*\\.[[:alnum:].-]*[[:alnum:]]+(/[^ ,]*)?")]
    email = [(Email, i "[^@ ]+@[^@ .]+\\.[^@ ]+[^ @.,]")]

    location =
      [ (Location, knownPlacesI),
        (Location, knownPlacesR)
      ]

    remoteness =
      [ (Remoteness, i "remote( *\\w*)?"),
        (Remoteness, i "remote( *\\([^)]*\\))?"),
        (Remoteness, i "hybrid|on-?site|in-?person")
      ]

    knownPlacesI :: Regex
    knownPlacesI =
      i
        $ intercalate "|"
          . map (\s -> "\\b" <> s <> "\\b")
        $ ["amsterdam", "netherlands", "berlin", "germany", "vienna", "austria", "bristol", "london", "uk", "canada", "nyc", "sf", "bay.?area", "global", "worldwide", "united states", "canada", "latin america", "europe", "lisbon", "portugal"]

    knownPlacesR :: Regex
    knownPlacesR =
      r
        $ intercalate "|"
          . map (\s -> "\\b" <> s <> "\\b")
        $ ["us"]

    purpose = [(Purpose, i "blockchain"), (Purpose, i "web3(.0)?"), (Purpose, i "nft")]

removeSubsumed :: [Salient] -> [Salient]
removeSubsumed sals = filter (not . subsumedByAny) sals
  where
    spans = map getSpan sals
    subsumedByAny sal = any (subsumes (getSpan sal)) spans
    subsumes (Span o1 l1) (Span o2 l2) =
      o2 <= o1 && o1 + l1 <= o2 + l2 && (o1, l1) /= (o2, l2)

getSpan :: Salient -> Span
getSpan = \case
  Location _ s -> s
  Salary _ s -> s
  Remoteness _ s -> s
  Tech _ s -> s
  URL _ s -> s
  Email _ s -> s
  Purpose _ s -> s
