module HNI.Fetch where

import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy as BS
import Debug.Trace
import HNI.Decoded
import HNI.Post
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory

-- https://hacker-news.firebaseio.com/v0/item/8863.json?print=pretty

fetch :: Int -> IO [Post Decoded]
fetch = fmap decode . get . toUrl
  where
    decode x = either (\e -> error $ ppError x e) (children . fmap HNI.Decoded.decode) . eitherDecode $ x

    ppError :: ByteString -> String -> String
    ppError bs e = unlines [show bs, e]

    baseUrl = "http://hn.algolia.com/api/v1/items/"
    toUrl id = baseUrl ++ show id

get :: String -> IO ByteString
get url = do
  manager <- newManager tlsManagerSettings

  request <- parseRequest url
  response <- httpLbs request manager

  return $ responseBody response

readCache :: Int -> IO (Maybe ByteString)
readCache n = do
  let fp = "/tmp/hn-cache/" ++ show n
  exists <- doesFileExist fp
  if exists
    then Just <$> BS.readFile fp
    else pure Nothing

writeCache :: Int -> ByteString -> IO ByteString
writeCache n bs = do
  createDirectoryIfMissing True "/tmp/hn-cache"
  BS.writeFile ("/tmp/hn-cache/" ++ show n) bs
  return bs

fetchCached :: Int -> IO [Post Decoded]
fetchCached n = do
  bs <- readCache n
  decode <$> maybe (trace "FETCHED" $ writeCache n =<< get (toUrl n)) pure bs
  where
    decode x = either (\e -> error $ ppError x e) (children . fmap HNI.Decoded.decode) . eitherDecode $ x

    ppError :: ByteString -> String -> String
    ppError bs e = unlines [show bs, e]

    baseUrl = "http://hn.algolia.com/api/v1/items/"
    toUrl id = baseUrl ++ show id
