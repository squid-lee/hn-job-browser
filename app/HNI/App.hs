{-# LANGUAGE RecordWildCards #-}

module HNI.App (newState, app) where

import Brick
import Brick.Highlight (txtWrapHighlight)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.Zipper
import qualified Data.Text as T
import Graphics.Vty (Event (..), Key (..))
import Graphics.Vty.Attributes
import HNI.Decoded
import HNI.Post
import HNI.PrettyPrint
import HNI.Salience
import System.Clipboard
import System.Process
import Text.Wrap (defaultWrapSettings)

data State = State
  { posts :: Zipper (Post Decoded)
  }

newState :: [Post Decoded] -> State
newState xs = State $ fromList xs

app :: App State event ()
app =
  App
    { appDraw = drawWidget,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const (attrMap defAttr $ zip highlightAttrNames $ cycle $ map bg [red, blue, green])
    }

appEvent :: BrickEvent () e -> EventM () State ()
appEvent (VtyEvent e) =
  case e of
    EvKey (KChar 'n') [] -> do
      modify $ \s -> s {posts = right $ posts s}
    EvKey (KChar 'p') [] -> do
      modify $ \s -> s {posts = left $ posts s}
    EvKey (KChar 'w') [] -> do
      s <- get
      liftIO $ setClipboardString $ T.unpack $ payload $ text $ cursor $ posts s
      continueWithoutRedraw
    EvKey (KChar 'o') [] -> do
      s <- get
      liftIO $ browse $ cursor $ posts s
      continueWithoutRedraw
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> halt
    _ -> return ()
appEvent _ = return ()

drawWidget :: State -> [Widget ()]
drawWidget s =
  pure $ case safeCursor $ posts s of
    Just p -> drawPost p
    Nothing -> txt "Seen all listings"

drawPost :: Post Decoded -> Widget ()
drawPost p@Post {..} =
  vBox $
    map
      (padLeftRight 2 . withBorderStyle unicode . border)
      [ vBox [txtWrap $ T.unwords [author, createdAt], txtWrapHighlight highlights defaultWrapSettings $ payload text],
        txtWrap $
          T.unlines . nub . map ppSalient $
            ss
      ]
  where
    highlights = color $ map getSpan ss
    ss = salients p

color :: [a] -> [(a, AttrName)]
color xs = zip xs $ cycle highlightAttrNames

highlightAttrNames :: [AttrName]
highlightAttrNames = map (attrName . ("highlight" ++) . show) [1 .. 4 :: Int]

browse :: Post a -> IO ()
browse Post {..} = do
  case parentId of
    Nothing -> return ()
    -- Injection possible (though pId / postId are Int)
    Just pId -> (>> return ()) $ spawnCommand $ concat ["xdg-open ", "https://news.ycombinator.com/item?id=", show pId, "#", show postId]
