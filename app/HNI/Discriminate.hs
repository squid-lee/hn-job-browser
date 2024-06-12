module HNI.Discriminate where

import Control.Applicative
import Data.Maybe
import HNI.Post
import Text.Regex.PCRE

-- TODO: Parameterise over more than just 'string'
regexp :: String -> Post String -> Maybe (Post String)
regexp regex p = p <$ (text p =~~ regex :: Maybe String)

-- TODO: Parameterise over more than just 'string'
regexen :: [String] -> Post String -> Maybe (Post String)
regexen rs p = case mapMaybe (\x -> text p =~~ x :: Maybe String) rs of
  [] -> Nothing
  _ -> Just p

remoteAvailable :: Post String -> Maybe (Post String)
remoteAvailable p = regexp "(?i)remote" p <|> regexp "hybrid" p
  where
    not m = maybe (Just p) (const Nothing)

data Certainty a = Uncertain | Certain a
  deriving (Eq, Ord, Show)
