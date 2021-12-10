module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.SuperBoard
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import Audio
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextSuperS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextSuperS s =<< liftIO (play X s)

  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move superUp s)   
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move superDown  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move superLeft  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move superRight s)

  T.VtyEvent (V.EvKey (V.KChar 'k') _) -> Brick.continue (move superUp s)   
  T.VtyEvent (V.EvKey (V.KChar 'j') _) -> Brick.continue (move superDown  s)
  T.VtyEvent (V.EvKey (V.KChar 'h') _) -> Brick.continue (move superLeft  s)
  T.VtyEvent (V.EvKey (V.KChar 'l') _) -> Brick.continue (move superRight s)

  T.VtyEvent (V.EvKey (V.KChar 'w') _) -> Brick.continue (move superUp s)   
  T.VtyEvent (V.EvKey (V.KChar 's') _) -> Brick.continue (move superDown  s)
  T.VtyEvent (V.EvKey (V.KChar 'a') _) -> Brick.continue (move superLeft  s)
  T.VtyEvent (V.EvKey (V.KChar 'd') _) -> Brick.continue (move superRight s)

  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: ((Pos, Pos) -> (Pos, Pos)) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psSuperPos = f (psSuperPos s) }

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result SuperBoard, Pos)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = do
    let posPair = getPosPair xo s
    (superRes, subRes) <- superPut (psSuperBoard s) xo <$> posPair

    case superRes of
      Retry -> playTrack "placeFail" s--playSoundPlaceFail 
      _     -> do
        playTrack "place" s-- playSoundPlace
        case subRes of 
          Win X -> playTrack "subWin" s --playSoundSubWin
          Win O -> playTrack "subLose" s --playSoundSubLose
          _     -> pure ()
        case superRes of
          Win X -> playTrack "superWin" s --playSoundSuperWin
          Win O -> playTrack "superLose" s --playSoundSuperLose
          _     -> pure ()

    pp <- posPair
    return (superRes, fst pp)

  | otherwise      = return (Retry, Pos 0 0)

getPosPair :: XO -> PlayState -> IO (Pos, Pos)
getPosPair xo s = getSuperStrategy xo s (psSuperPos s) (psSuperBoard s) xo (psLastSuper s)

-------------------------------------------------------------------------------
nextSuperS :: PlayState -> (Result SuperBoard, Pos) -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextSuperS s (b, lastSuper) = case next s b of
  Right s' -> continue (s' {psLastSuper = lastSuper} )
  Left res -> halt (s { psResult = res }) 

getSuperStrategy :: XO -> PlayState -> SuperStrategy 
getSuperStrategy X s = plStrat (psX s)
getSuperStrategy O s = plStrat (psO s)
