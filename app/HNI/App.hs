module HNI.App where

import Brick
import Data.List.Zipper
-- import Data.Text

import Graphics.Vty (defAttr)
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
      appHandleEvent = \_ -> return (),
      appStartEvent = return (),
      appAttrMap = const (attrMap defAttr [])
    }

drawWidget :: State -> [Widget ()]
drawWidget s = [str . ppPost . head . toList . posts $ s]
