module Main where

import Brick
import qualified Brick.Util as BU
import Graphics.Vty.Attributes
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
import Sound.ALUT

-------------------------------------------------------------------------------
main :: IO ()
main = do
  difficulty <- fromMaybe defaultDifficulty <$> getDifficulty
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  -- run sound context
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
    do
      (Just device) <- openDevice Nothing
      (Just context) <- createContext device []
      currentContext $= Just context
      res <- customMain initialVty buildVty (Just chan) app (Intro difficulty)
      case res of
        Play res -> print (psResult res, psScore res) 
        _ -> return ()
      closeDevice device
      return ()

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

getDifficulty :: IO (Maybe Int)
getDifficulty = do
  args <- getArgs
  case args of
    (str:_) -> return (readMaybe str)
    _       -> return Nothing

defaultDifficulty :: Int
defaultDifficulty = 1
