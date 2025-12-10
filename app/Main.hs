module Main where

import Brick (defaultMain)
import HNI.App
import HNI.Fetch

main :: IO ()
main = do
  let jul24 = 40846428
  posts <- fetchCached jul24
  _ <- defaultMain app $ newState posts
  return ()
