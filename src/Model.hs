{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import qualified Model.SuperBoard as SuperBoard
import Sound.ALUT
import Audio
import Data.Map as M

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psX           :: Player.Player   -- ^ player X info
  , psO           :: Player.Player   -- ^ player O info
  , psScore       :: Score.Score     -- ^ current score
  , psBoard       :: Board.Board     -- ^ current board
  , psSuperBoard  :: SuperBoard.SuperBoard
  , psSuperPos    :: (Board.Pos, Board.Pos)
  , psTurn        :: Board.XO        -- ^ whose turn 
  , psPos         :: Board.Pos       -- ^ current cursor
  , psResult      :: Board.Result () -- ^ result      
  , psSounds      :: TrackMap
  -- , psLastXsuper  :: Board.Pos
  -- , psLastOsuper  :: Board.Pos
  } 

init :: Int -> PlayState
init n = PS 
  { psX           = Player.human
  , psO           = Player.ai
  , psScore       = Score.init n
  , psBoard       = Board.init
  , psSuperBoard  = SuperBoard.superBoardInit 3
  , psSuperPos    = (head (SuperBoard.superPositions 3), head Board.positions)
  , psTurn        = Board.X
  , psPos         = head Board.positions 
  , psResult      = Board.Cont ()
  , psSounds      = loadTracks
  -- , psLastXsuper  = Board.Pos 0 0
  -- , psLastOsuper  = Board.Pos 0 0
  }

-- play a sound synchronously, given name of the track
--   leaked from Audio.hs to here
--   because we need PlayState
playTrack :: String -> PlayState -> IO ()
playTrack name ps = do
  case M.lookup name (psSounds ps) of
    Just source -> do
      src <- source
      Sound.ALUT.play [src]
    _ -> pure ()

-- isCurr :: PlayState -> Int -> Int -> Bool
-- isCurr s r c = Board.pRow p == r && Board.pCol p == c
--   where 
--     p = psPos s 

--- >>> (Model.init 2)
--- <interactive>:16987:2-15: error:
---     • No instance for (Show PlayState) arising from a use of ‘print’
---     • In a stmt of an interactive GHCi command: print it
---

isCurr :: PlayState -> Int -> Int -> Int -> Int -> Bool
isCurr s supR supC subR subC = Board.pRow supPos == supR && Board.pCol supPos == supC && Board.pRow subPos == subR && Board.pCol subPos == subC
  where 
    subPos = snd (psSuperPos s)
    supPos = fst (psSuperPos s)

next :: PlayState -> Board.Result SuperBoard.SuperBoard -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psSuperBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s res             = nextBoard s res 


-- next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
-- next s Board.Retry     = Right s
-- next s (Board.Cont b') = Right (s { psBoard = b'
--                                   , psTurn  = Board.flipXO (psTurn s) })
-- next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psSuperBoard = SuperBoard.superBoardInit 3
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } 

-- nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
-- nextBoard s res = case res' of
--                     Board.Win _ -> Left res' 
--                     Board.Draw  -> Left res'
--                     _           -> Right s' 
--   where 
--     sc'  = Score.add (psScore s) (Board.boardWinner res) 
--     res' = Score.winner sc'
--     s'   = s { psScore = sc'                   -- update the score
--              , psBoard = mempty                -- clear the board
--              , psTurn  = Score.startPlayer sc' -- toggle start player
--              } 

