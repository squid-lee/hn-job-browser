{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module HNI.App (newState, app) where

import Brick
import Brick.Highlight (txtWrapHighlight)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Monad (when)
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
  { posts :: Zipper (Post Decoded),
    jumpBuffer :: Int
  }

newState :: [Post Decoded] -> State
newState xs = State {posts = fromList xs, jumpBuffer = 0}

app :: App State event ()
app =
  App
    { appDraw = drawWidget,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const (attrMap defAttr $ zip highlightAttrNames $ cycle $ map bg [red, blue, green])
    }

-- Jump to absolute position (0-indexed)
jumpTo :: Int -> Zipper a -> Zipper a
jumpTo n z = let z' = start z in iterate right z' !! n

-- Get current position (1-indexed) and total count
getPosition :: Zipper a -> (Int, Int)
getPosition z =
  let totalCount = length $ toList z
      -- Count backwards to get position
      countBack zp acc
        | beginp zp = acc
        | otherwise = countBack (left zp) (acc + 1)
      currentPos = countBack z 1
  in (currentPos, totalCount)

appEvent :: BrickEvent () e -> EventM () State ()
appEvent (VtyEvent e) =
  case e of
    EvKey (KChar 'n') [] -> do
      modify $ \s -> s {posts = right $ posts s, jumpBuffer = 0}
    EvKey (KChar 'p') [] -> do
      modify $ \s -> s {posts = left $ posts s, jumpBuffer = 0}
    EvKey (KChar 'w') [] -> do
      s <- get
      liftIO $ setClipboardString $ T.unpack $ payload $ text $ cursor $ posts s
      continueWithoutRedraw
    EvKey (KChar 'o') [] -> do
      s <- get
      liftIO $ browse $ cursor $ posts s
      continueWithoutRedraw
    EvKey (KChar 'g') [] -> do
      s <- get
      let targetPos = jumpBuffer s - 1
      when (targetPos >= 0) $
        modify $ \st -> st {posts = jumpTo targetPos $ posts st, jumpBuffer = 0}
    EvKey (KChar c) [] | c >= '0' && c <= '9' -> do
      let digit = read [c] :: Int
      modify $ \s -> s {jumpBuffer = jumpBuffer s * 10 + digit}
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> halt
    _ -> return ()
appEvent _ = return ()

drawWidget :: State -> [Widget ()]
drawWidget s =
  pure $ case safeCursor $ posts s of
    Just p -> vBox [drawPost p, drawFooter s]
    Nothing -> txt "Seen all listings"

drawFooter :: State -> Widget ()
drawFooter s =
  let (current, total) = getPosition (posts s)
      counterText = T.pack $ show current ++ " / " ++ show total
      bufferText = if jumpBuffer s > 0 then T.pack (show (jumpBuffer s)) else ""
  in hBox [fill ' ', txt bufferText, txt " ", txt counterText]

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
