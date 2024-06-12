module HNI.App (newState, app) where

import Brick
import Data.List.Zipper
-- import Data.Text

import Graphics.Vty (Event (..), Key (..), defAttr)
import HNI.Post
import HNI.PrettyPrint

data State = State
  { -- TODO Zipper
    posts :: Zipper (Post String)
  }

newState :: [Post String] -> State
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
drawWidget s = [str . ppPost . cursor . posts $ s]

appEvent :: BrickEvent () e -> EventM () State ()
appEvent (VtyEvent e) =
  case e of
    EvKey (KChar 'n') [] ->
      modify $ \s -> s {posts = right $ posts s}
    EvKey (KChar 'p') [] ->
      modify $ \s -> s {posts = left $ posts s}
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> halt
    _ -> return ()
appEvent _ = return ()
