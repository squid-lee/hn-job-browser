{-# LANGUAGE DeriveGeneric #-}

module HNI.Post where

import Data.Aeson
import GHC.Generics

-- The top level Post (ie the whole jobs thread) is also a Post
data Post body = Post
  { author :: String,
    children :: [Post body],
    createdAt :: String,
    createdAtI :: Int,
    postId :: Int,
    options :: [()],
    parentId :: Maybe Int,
    points :: Maybe Int,
    storyId :: Int,
    text :: body,
    title :: Maybe String,
    typ :: String,
    url :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance (FromJSON body) => FromJSON (Post body) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . renaming [("postId", "id"), ("typ", "type")]}

renaming :: [(String, String)] -> String -> String
renaming table s = maybe s (\x -> x) $ lookup s table
