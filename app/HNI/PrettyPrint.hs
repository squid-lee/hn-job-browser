{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HNI.PrettyPrint where

import Data.Text (Text)
import qualified Data.Text as T
import HNI.Decoded
import HNI.Post
import HNI.Salience

-- TODO polymorphic over body:
ppPost :: Post Decoded -> Text
ppPost Post {..} = T.unlines [T.unwords [author, createdAt], payload text] -- {text = ppText (text post)}

ppSalient :: Salient -> Text
ppSalient =
  pad
    . fmap T.strip
    . \case
      Location x -> ("Location", x)
      Salary x -> ("Salary", x)
      Remoteness x -> ("Remoteness", T.toTitle x)
      Tech x -> ("Tech", x)
      URL x -> ("URL", x)
      Email x -> ("Email", x)
  where
    ctors = ["Location", "Salary", "Remoteness", "Tech", "URL", "Email"]
    len = maximum $ map T.length ctors
    pad (ctor, val) = T.justifyLeft len ' ' ctor <> " " <> val
