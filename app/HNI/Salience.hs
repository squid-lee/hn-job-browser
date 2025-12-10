{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}

module HNI.Salience where

import Brick.Span
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
  concatMap (\(mkTag, regexp) -> map (uncurry mkTag . toSpan) $ matchAllText regexp (payload (text p)))
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

    -- 80k £80k 80k€ 80-130k€ £80k-£130k 80k-130k
    salary =
      [ (Salary, i "([£$€][[:digit:]]{2,}k?)|([[:digit:]]{2,}[k€£$]+)"),
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

getSpan :: Salient -> Span
getSpan = \case
  Location _ s -> s
  Salary _ s -> s
  Remoteness _ s -> s
  Tech _ s -> s
  URL _ s -> s
  Email _ s -> s
  Purpose _ s -> s
