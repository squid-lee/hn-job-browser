{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module HNI.Salience where

import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import HNI.Decoded
import HNI.Post
import Text.Regex.TDFA

data Salient
  = Location Text
  | Salary Text
  | Remoteness Text
  | Tech Text
  | URL Text
  | Email Text
  deriving (Generic, Eq, Ord, Show)

-- pack is required because PCRE
salients :: Post Decoded -> [Salient]
salients p = concatMap (mapMaybe (\(mkTag, regexp) -> mkTag <$> matchM (make regexp) (payload (text p)))) [remoteness, salary, url, email]
  where
    -- mk :: String -> Regex
    make :: String -> Regex
    make r = makeRegexOpts (defaultCompOpt {caseSensitive = False}) defaultExecOpt r

    remoteness = [(Location, "remote"), (Location, "remote|hybrid|on-?site")]
    salary = [(Salary, "[\\d,.]*k?[£$€][\\d,.]*k?"), (Salary, "equity")]
    url = [(URL, "https?://[[:alnum:]]*\\.[[:alnum:].]*(/[^ ])?")]
    email = [(Email, "[^@ ]+@[^@ .]+\\.[^@ ]+")]
