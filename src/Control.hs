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

control :: State -> BrickEvent n Tick -> EventM n (Next State)
control s (T.VtyEvent (V.EvKey V.KEsc _)) = Brick.halt s
control (Play s) ev = case ev of
  AppEvent Tick                   -> nextSuperS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextSuperS s =<< liftIO (play X s)

  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue $ Play (move superUp s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue $ Play (move superDown  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue $ Play (move superLeft  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue $ Play (move superRight s)

  T.VtyEvent (V.EvKey (V.KChar 'k') _) -> Brick.continue $ Play (move superUp s)
  T.VtyEvent (V.EvKey (V.KChar 'j') _) -> Brick.continue $ Play (move superDown  s)
  T.VtyEvent (V.EvKey (V.KChar 'h') _) -> Brick.continue $ Play (move superLeft  s)
  T.VtyEvent (V.EvKey (V.KChar 'l') _) -> Brick.continue $ Play (move superRight s)

  T.VtyEvent (V.EvKey (V.KChar 'w') _) -> Brick.continue $ Play (move superUp s)
  T.VtyEvent (V.EvKey (V.KChar 's') _) -> Brick.continue $ Play (move superDown  s)
  T.VtyEvent (V.EvKey (V.KChar 'a') _) -> Brick.continue $ Play (move superLeft  s)
  T.VtyEvent (V.EvKey (V.KChar 'd') _) -> Brick.continue $ Play (move superRight s)

  _                               -> Brick.continue $ Play s -- Brick.halt s
control s@(Outro _) _ = Brick.continue s
control (Intro r) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue $ Play (Model.init r)
  _ -> Brick.continue $ Intro r
-------------------------------------------------------------------------------
move :: ((Pos, Pos) -> (Pos, Pos)) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psSuperPos = f (psSuperPos s) }

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result SuperBoard)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = do
    (superRes, subRes) <- superPut (psSuperBoard s) xo <$> getPosPair xo s

    case superRes of
      Retry -> playSoundPlaceFail 
      _     -> do
        playSoundPlace
        case subRes of 
          Win X -> playSoundSubWin
          Win O -> playSoundSubLose
          _     -> pure ()
        case superRes of
          Win X -> playSoundSuperWin
          Win O -> playSoundSuperLose
          _     -> pure ()

    return superRes

  | otherwise      = return Retry

getPosPair :: XO -> PlayState -> IO (Pos, Pos)
getPosPair xo s = getSuperStrategy xo s (psSuperPos s) (psSuperBoard s) xo 

-------------------------------------------------------------------------------
nextSuperS :: PlayState -> Result SuperBoard -> EventM n (Next State)
-------------------------------------------------------------------------------
nextSuperS s b = case next s b of
  Right s' -> continue $ Play s'
  Left res -> continue (Outro res)

getSuperStrategy :: XO -> PlayState -> SuperStrategy 
getSuperStrategy X s = plStrat (psX s)
getSuperStrategy O s = plStrat (psO s)


-- getPos :: XO -> PlayState -> IO Pos
-- getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

-- getStrategy :: XO -> PlayState -> Strategy 
-- getStrategy X s = plStrat (psX s)
-- getStrategy O s = plStrat (psO s)

-- -------------------------------------------------------------------------------
-- nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-- -------------------------------------------------------------------------------
-- nextS s b = case next s b of
--   Right s' -> continue s'
--   Left res -> halt (s { psResult = res }) 


