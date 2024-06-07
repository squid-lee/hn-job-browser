module HNI.Discriminate where

import HNI.Post
import Text.Regex.PCRE

-- TODO: Parameterise over more than just 'string'
regexp :: String -> Post String -> Maybe (Post String)
regexp regex p = p <$ (text p =~~ regex :: Maybe String)
