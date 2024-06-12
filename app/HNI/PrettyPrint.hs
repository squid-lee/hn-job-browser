{-# LANGUAGE RecordWildCards #-}

module HNI.PrettyPrint where

import Data.List
import HNI.Post
import Network.URI

-- TODO polymorphic over body:
ppPost :: Post String -> String
ppPost Post {..} = unlines [unwords [author, createdAt], ppText $ text] -- {text = ppText (text post)}

ppText :: String -> String
ppText = replacing [("&quot;", "'"), ("&amp;", "&"), ("&#x2F;", "/"), ("&#x27;", "'")] . removeTags . replacing [("<p>", ['\n'])]
  where
    removeTags "" = ""
    removeTags ('<' : s) = removeTags $ drop 1 . dropWhile (/= '>') $ s
    removeTags (x : s) = x : removeTags s

-- TODO: SLOW
replacing :: [(String, String)] -> String -> String
replacing tblTop s = go tblTop s
  where
    go [] "" = ""
    go [] (x : xs) = x : go tblTop xs
    go ((pref, replace) : tbl) s =
      case stripPrefix pref s of
        Nothing -> go tbl s
        Just s' -> replace ++ go tblTop s'
