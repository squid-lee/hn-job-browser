{-# LANGUAGE DeriveGeneric #-}

module HNI.App (newState, app) where

import Brick
-- import Data.Text

import Control.Monad.IO.Class (liftIO)
import Data.List.Zipper
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty (Event (..), Key (..), defAttr)
import HNI.Post
import HNI.PrettyPrint
import HNI.Salience
import System.Clipboard

data State = State
  { posts :: Zipper (Post Text)
  }

newState :: [Post Text] -> State
newState xs = State $ fromList xs

app :: App State event ()
app =
  App
    { appDraw = drawWidget,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const (attrMap defAttr [])
    }

drawWidget :: State -> [Widget ()]
drawWidget s =
  pure $
    vBox
      [ txtWrap . ppPost $ p,
        txtWrap . T.pack . unlines . map show . salients $ p
      ]
  where
    p = cursor . posts $ s

appEvent :: BrickEvent () e -> EventM () State ()
appEvent (VtyEvent e) =
  case e of
    EvKey (KChar 'n') [] ->
      modify $ \s -> s {posts = right $ posts s}
    EvKey (KChar 'p') [] ->
      modify $ \s -> s {posts = left $ posts s}
    EvKey (KChar 'w') [] -> do
      s <- get
      liftIO $ setClipboardString $ T.unpack $ text $ cursor $ posts s
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> halt
    _ -> return ()
appEvent _ = return ()
