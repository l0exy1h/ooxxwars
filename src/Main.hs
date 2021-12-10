module Main where

import Brick
import qualified Brick.Util as BU
import Graphics.Vty.Attributes
import Graphics.Vty
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View 
import Control 
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  rounds <- fromMaybe defaultRounds <$> getRounds
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <- customMain initialVty buildVty (Just chan) app (Intro rounds)
  case res of
    Play res -> print (psResult res, psScore res) 
    _ -> return ()

app :: App State Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const attrmap -- const (attrMap defAttr [])
  }
    where
      blueBg = attrName "blueBg"
      redBg = attrName "redBg"
      attrmap = attrMap defAttr [(blueBg, BU.bg blue), (redBg, BU.bg red)]

getRounds :: IO (Maybe Int)
getRounds = do
  args <- getArgs
  case args of
    (str:_) -> return (readMaybe str)
    _       -> return Nothing

defaultRounds :: Int
defaultRounds = 1
