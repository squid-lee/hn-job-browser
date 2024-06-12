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
  deriving (Generic, Eq, Ord, Show)

salients :: Post String -> [Salient]
salients p = concatMap (mapMaybe (\(mkTag, regexp) -> mkTag <$> text p =~~ regexp)) [remoteness, salary]
  where
    remoteness = [(\match -> Location match, "(?i)remote *\\(.*?\\)"), (\match -> Location match, "(?i)remote")]
    salary = [(\match -> Salary match, "[\\d,.]*k?[£$€][\\d,.]*")]
