{-# LANGUAGE LambdaCase #-}

module Main where

import Brick (defaultMain)
import Data.Char (isDigit)
import Data.List (dropWhileEnd, isInfixOf, nub)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HNI.App
import HNI.Decoded
import HNI.Fetch
import HNI.Post
import HNI.PrettyPrint
import HNI.Salience
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (flags, rest) = span ("-" `isPrefixOf`) args
      manchester = any (`elem` ["-m", "--manchester"]) flags
      queryMode = any (`elem` ["-q", "--query"]) flags
  itemId <- case rest of
    [] -> error "Usage: hacker-news [-m|--manchester] [-q|--query] <url-or-id>"
    (arg : _) -> return $ parseHnArg arg
  posts <- fetchCached itemId
  let filtered = if manchester then filter manchesterFriendly posts else posts
  if queryMode
    then mapM_ printPost filtered
    else do
      _ <- defaultMain app $ newState filtered
      return ()

printPost :: Post Decoded -> IO ()
printPost p = do
  TIO.putStrLn $ T.unwords ["---", author p, "---"]
  TIO.putStrLn $ payload $ text p
  TIO.putStrLn ""
  TIO.putStrLn $ T.unlines . nub . map ppSalient $ salients p
  TIO.putStrLn ""

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

parseHnArg :: String -> Int
parseHnArg s
  | all isDigit s = read s
  | "id=" `isInfixOf` s = read $ takeWhile isDigit $ drop 1 $ dropWhile (/= '=') $ dropWhileEnd (not . isDigit) s
  | otherwise = error $ "Cannot parse HN URL or ID: " ++ s

manchesterFriendly :: Post Decoded -> Bool
manchesterFriendly post = hasRemote || hasNearbyLocation
  where
    sals = salients post
    texts = [T.toLower t | sal <- sals, let t = salText sal]

    hasRemote = any isGoodRemote texts
    hasNearbyLocation = any isNearby texts

    isGoodRemote t =
      "remote" `T.isInfixOf` t
        && not ("us only" `T.isInfixOf` t || "us-only" `T.isInfixOf` t)

    isNearby t = any (`T.isInfixOf` t) nearbyPlaces
    nearbyPlaces =
      [ "uk",
        "london",
        "manchester",
        "bristol",
        "birmingham",
        "leeds",
        "edinburgh",
        "glasgow",
        "dublin",
        "ireland",
        "amsterdam",
        "netherlands",
        "paris",
        "france",
        "brussels",
        "belgium",
        "ghent",
        "europe"
      ]

salText :: Salient -> T.Text
salText = \case
  Location t _ -> t
  Salary t _ -> t
  Remoteness t _ -> t
  Tech t _ -> t
  Role t _ -> t
  URL t _ -> t
  Email t _ -> t
  Purpose t _ -> t
