module Main where

import Brick (defaultMain)
import Data.Char (isDigit)
import Data.List (dropWhileEnd, isInfixOf)
import HNI.App
import HNI.Fetch
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  itemId <- case args of
    [] -> error "Usage: hacker-news <url-or-id>"
    (arg : _) -> return $ parseHnArg arg
  posts <- fetchCached itemId
  _ <- defaultMain app $ newState posts
  return ()

parseHnArg :: String -> Int
parseHnArg s
  | all isDigit s = read s
  | "id=" `isInfixOf` s = read $ takeWhile isDigit $ drop 1 $ dropWhile (/= '=') $ dropWhileEnd (not . isDigit) s
  | otherwise = error $ "Cannot parse HN URL or ID: " ++ s
