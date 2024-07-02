{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
  concatMap (\(mkTag, regexp) -> map (uncurry mkTag . toSpan) $ matchAllText (make regexp) (payload (text p)))
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

    make :: String -> Regex
    make r = makeRegexOpts (defaultCompOpt {caseSensitive = False}) defaultExecOpt r

    -- 80k £80k 80k€ 80-130k€ £80k-£130k 80k-130k
    salary =
      [ (Salary, "([£$€][[:digit:]]{2,}k?)|([[:digit:]]{2,}[k€£$]+)"),
        (Salary, "equity")
      ]
    url = [(URL, "https?://[[:alnum:]]*\\.[[:alnum:].]*[[:alnum:]]+(/[^ ]*)?")]
    email = [(Email, "[^@ ]+@[^@ .]+\\.[^@ ]+[^ @.]")]

    location =
      [ (Location, knownPlaces)
      ]

    remoteness =
      [ (Remoteness, "remote( *\\w*)?"),
        (Remoteness, "remote( *\\([^)]*\\))?"),
        (Remoteness, "hybrid|on-?site|in-?person")
      ]

    knownPlaces :: String
    knownPlaces =
      intercalate "|"
        . map (\s -> "\\b" <> s <> "\\b")
        $ ["amsterdam", "netherlands", "berlin", "germany", "vienna", "austria", "bristol", "london", "uk", "canada", "nyc", "sf", "bay.?area", "global", "worldwide", "us", "united states", "canada", "latin america", "europe", "lisbon", "portugal"]

    purpose = [(Purpose, "blockchain"), (Purpose, "web3(.0)?"), (Purpose, "nft")]

getSpan :: Salient -> Span
getSpan = \case
  Location _ s -> s
  Salary _ s -> s
  Remoteness _ s -> s
  Tech _ s -> s
  URL _ s -> s
  Email _ s -> s
  Purpose _ s -> s
