module Main where

import Brick
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import HNI.App
import HNI.Decoded
import HNI.Post

main :: IO ()
main = main1

main1 :: IO ()
main1 = do
  -- as <- getArgs
  -- (json, filter) <- case as of
  --   [f] -> (,id) <$> BS.readFile f
  --   [f, strs] -> (,mapMaybe (regexen (split ',' strs))) <$> BS.readFile f
  --   _ -> error "OH NO"

  json <- BS.readFile "/tmp/hn-jun24.json"

  posts <- either error (pure . children . fmap HNI.Decoded.decode) (eitherDecode json)

  _ <- defaultMain app $ newState posts
  return ()
