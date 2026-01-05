module Main where

import Brick (defaultMain)
import Data.Char (isDigit)
import Data.List (dropWhileEnd, isInfixOf, nub, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HNI.App
import HNI.Decoded
import HNI.Features (features)
import HNI.Fetch
import HNI.Post
import HNI.PrettyPrint (ppFeature)
import HNI.Rank
import System.Environment (getArgs)

-- 46108941 is Dec 25
-- 46466074 is Jan 26

main :: IO ()
main = do
  args <- getArgs
  let (flags, rest) = span ("-" `isPrefixOf`) args
      queryMode = any (`elem` ["-q", "--query"]) flags
  itemId <- case rest of
    [] -> error "Usage: hacker-news [-m|--manchester] [-q|--query] <url-or-id>"
    (arg : _) -> return $ parseHnArg arg

  posts <- fetchCached itemId

  let thePosts = sortOn (Down . rank . features) posts

  if queryMode
    then mapM_ printPost thePosts
    else do
      _ <- defaultMain app $ newState thePosts
      return ()

printPost :: Post Decoded -> IO ()
printPost p = do
  TIO.putStrLn $ T.unwords ["---", author p, "---"]
  TIO.putStrLn $ payload $ text p
  TIO.putStrLn ""
  TIO.putStrLn $ T.unlines . nub . map ppFeature $ features p
  TIO.putStrLn ""

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

parseHnArg :: String -> Int
parseHnArg s
  | all isDigit s = read s
  | "id=" `isInfixOf` s = read $ takeWhile isDigit $ drop 1 $ dropWhile (/= '=') $ dropWhileEnd (not . isDigit) s
  | otherwise = error $ "Cannot parse HN URL or ID: " ++ s
