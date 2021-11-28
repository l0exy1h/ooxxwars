module View
  ( view
  ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center           ( center )
import           Text.Printf                    ( printf )

import qualified Data.Map                      as M
import           Data.Maybe
import           Graphics.Vty            hiding ( dim )
import           Model
import           Model.Board

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = withBorderStyle (borderStyleFromChar '█') $ borderWithLabel (str (header s)) $ vTile
  [ mkRow s row | row <- [1 .. dim] ]

header :: PlayState -> String
header s = printf "Tic-Tac-Toe Turn = %s, row = %d, col = %d" (show (psTurn s)) (pRow p) (pCol p)
  where p = psPos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkBoard s row i | i <- [1 .. dim] ]

mkBoard :: PlayState -> Int -> Int -> Widget n
mkBoard s sr sc = colored bordered
  where
    (colored, vstack, hstack) = case getBoardResult subBoard of
      Win X -> (blued, vBox, hBox)
      Win O -> (reded, vBox, hBox)
      _     -> (id, vTile, hTile)
    origBoard = vstack [ hstack [ mkCell s sr sc row i | i <- [1 .. dim] ] | row <- [1 .. dim] ]
    bordered  = withBorderStyle unicode origBoard
    blued     = withAttr (attrName "blueBg")
    reded     = withAttr (attrName "redBg")
    subBoard  = fromJust $ M.lookup (Pos sr sc) (psSuperBoard s)

mkCell :: PlayState -> Int -> Int -> Int -> Int -> Widget n
mkCell s sr sc r c = raw where raw = mkCell' s sr sc r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Int -> Int -> Widget n
mkCell' s sr sc r c = center (mkXO xoMb)
  where xoMb = fromJust (M.lookup (Pos sr sc) (psSuperBoard s)) ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO

blockB, blockX, blockO :: Widget n
blockB = vBox (replicate 3 (str "   "))
blockX = vBox [str "\\ /", str " ╳ ", str "/ \\"]
blockO = vBox [str "/‾\\", str "⎸ ⎹", str "\\_/"]

vTile :: [Widget n] -> Widget n
vTile (b : bs) = vBox (b : [ hBorder <=> b | b <- bs ])
vTile _        = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b : bs) = hBox (b : [ vBorder <+> b | b <- bs ])
hTile _        = emptyWidget

