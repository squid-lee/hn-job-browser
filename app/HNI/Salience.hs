{-# LANGUAGE DeriveGeneric #-}

module HNI.Salience where

import Data.Maybe
import GHC.Generics
import HNI.Post
import Text.Regex.PCRE

data Salient
  = Location String
  | Salary String
  | Remoteness String
  | Tech String
  | URL String
  | Email String
  deriving (Generic, Eq, Ord, Show)

salients :: Post String -> [Salient]
salients p = concatMap (mapMaybe (\(mkTag, regexp) -> mkTag <$> text p =~~ regexp)) [remoteness, salary, url, email]
  where
    remoteness = [(Location, "(?i)remote *\\(.*?\\)"), (Location, "(?i)remote|hybrid|on-?site")]
    salary = [(Salary, "[\\d,.]*[kK]?[£$€][\\d,.]*[kK]?"), (Salary, "(?i)equity")]
    url = [(URL, "(https?://(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?://(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})")]
    email = [(Email, "[^@ ]+@[^@ ]+\\.[^@ ]+")]
