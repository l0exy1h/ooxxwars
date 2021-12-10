module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.SuperBoard
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player

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

  _ -> Brick.continue $ Play s -- Brick.halt s
control s@(Outro _) _  = Brick.continue s
control (  Intro r) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue $ Play (Model.init r)
  _                               -> Brick.continue $ Intro r
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
      Retry -> playTrack "placeFail" s
      _     -> do
        playTrack "place" s
        case subRes of
          Win X -> playTrack "subWin" s
          Win O -> playTrack "subLose" s
          _     -> pure ()
        case superRes of
          Win X -> playTrack "superWin" s
          Win O -> playTrack "superLose" s
          _     -> pure ()
    pp <- posPair
    return (superRes, fst pp)
  | otherwise = return (Retry, Pos 0 0)

getPosPair :: XO -> PlayState -> IO (Pos, Pos)
getPosPair xo s = getSuperStrategy xo s (psSuperPos s) (psSuperBoard s) xo (psLastSuper s)

-------------------------------------------------------------------------------
nextSuperS :: PlayState -> (Result SuperBoard, Pos) -> EventM n (Next State)
-------------------------------------------------------------------------------
nextSuperS s (b, lastSuper) = case next s b of
  Right s'  -> continue $ Play (s' { psLastSuper = lastSuper })
  Left  res -> continue (Outro res)

getSuperStrategy :: XO -> PlayState -> SuperStrategy
getSuperStrategy X s = plStrat (psX s)
getSuperStrategy O s = plStrat (psO s)
