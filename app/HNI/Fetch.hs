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

baseUrl :: String
baseUrl = "http://hn.algolia.com/api/v1/items/"

toUrl :: Int -> String
toUrl n = baseUrl ++ show n

get :: String -> IO ByteString
get url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response

decodeJSON :: ByteString -> [Post Decoded]
decodeJSON x = either (\e -> error $ unlines [show x, e]) (children . fmap HNI.Decoded.decode) $ eitherDecode x

fetch :: Int -> IO [Post Decoded]
fetch = fmap decodeJSON . get . toUrl

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
  cached <- readCache n
  case cached of
    Just bs -> return $ decodeJSON bs
    Nothing -> trace "FETCHED" $ do
      bs <- get (toUrl n)
      _ <- writeCache n bs
      return $ decodeJSON bs
