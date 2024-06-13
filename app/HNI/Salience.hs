{-# LANGUAGE DeriveGeneric #-}

module HNI.Salience where

import Data.Maybe
import Data.Text (Text, pack, unpack)
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
salients p = concatMap (mapMaybe (\(mkTag, regexp) -> mkTag <$> (payload (text p) =~~ (regexp :: String)))) [remoteness, salary, url, email]
  where
    remoteness = [(Location, "(?i)remote *\\(.*?\\)"), (Location, "(?i)remote|hybrid|on-?site")]
    salary = [(Salary, "[\\d,.]*[kK]?[£$€][\\d,.]*[kK]?"), (Salary, "(?i)equity")]
    url = [(URL, "(https?://(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?://(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})")]
    email = [(Email, "[^@ ]+@[^@ .]+\\.[^@ ]+")]

emailRegexp :: String
emailRegexp = "[^@ ]+@[^@ ]+\\.[^@ ]+"

emailText :: String
emailText = "You can reach me at [my first name]@slay.cool or submit your application at"

match :: Maybe String
match = emailText =~~ emailRegexp
