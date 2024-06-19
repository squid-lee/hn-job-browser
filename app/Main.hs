{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
module Main where

import Brick
import Brick.Highlight (txtWrapHighlight)
import Brick.Span
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Graphics.Vty.Attributes.Color
import HNI.App
import HNI.Decoded
import HNI.Post
import Text.Wrap (defaultWrapSettings)

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

main2 :: IO ()
main2 = defaultMain app ()
  where
    xs :: Widget ()
    xs = txtWrapHighlight [(Span 10 10, attrName "X"), (Span 32 7, attrName "X"), (Span 40 10, attrName "X")] defaultWrapSettings (T.replicate 1 (T.replicate 80 "X" <> " "))

    app = App (\_ -> [xs]) neverShowCursor (\_ -> halt) (return ()) (const (attrMap (fg blue) [(attrName "X", fg red)]))
