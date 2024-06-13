{-# LANGUAGE DeriveGeneric #-}

module HNI.Post where

import Data.Aeson
import Data.Text
import GHC.Generics

-- The top level Post (ie the whole jobs thread) is also a Post
data Post body = Post
  { author :: Text,
    children :: [Post body],
    createdAt :: Text,
    createdAtI :: Int,
    postId :: Int,
    options :: [()],
    parentId :: Maybe Int,
    points :: Maybe Int,
    storyId :: Int,
    text :: body,
    title :: Maybe Text,
    typ :: Text,
    url :: Maybe Text
  }
  deriving (Generic, Show, Eq, Ord)

instance (FromJSON body) => FromJSON (Post body) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . renaming [("postId", "id"), ("typ", "type")]}

renaming :: [(String, String)] -> String -> String
renaming table s = maybe s (\x -> x) $ lookup s table
