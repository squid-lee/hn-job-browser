{-# LANGUAGE LambdaCase #-}

module HNI.PrettyPrint where

import Data.Text (Text)
import qualified Data.Text as T
import HNI.Salience

ppSalient :: Salient -> Text
ppSalient =
  pad
    . fmap T.strip
    . \case
      Location x _ -> ("Location", x)
      Salary x _ -> ("Salary", x)
      Remoteness x _ -> ("Remoteness", T.toTitle x)
      Tech x _ -> ("Tech", x)
      URL x _ -> ("URL", x)
      Email x _ -> ("Email", x)
      Purpose x _ -> ("Purpose", x)
  where
    ctors = ["Location", "Salary", "Remoteness", "Tech", "URL", "Email"]
    len = maximum $ map T.length ctors
    pad (ctor, val) = T.justifyLeft len ' ' ctor <> " " <> val
