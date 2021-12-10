module Model.Player where

import qualified Data.Map as M 
import Data.List as L
import Model.Board
import Model.SuperBoard
import System.Random -- (Random(randomRIO))
import Data.Maybe

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { plName  :: String 
  , plStrat :: SuperStrategy
  }

type Strategy = Pos     -- ^ current cursor
             -> Board   -- ^ current board
             -> XO      -- ^ naught or cross
             -> IO Pos  -- ^ next move

type SuperStrategy = (Pos, Pos)     -- ^ current cursor
             -> SuperBoard   -- ^ current board
             -> XO      -- ^ naught or cross
             -> IO (Pos, Pos)  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return p)

rando :: Player 
rando = Player "machine" randomStrategy

randomStrategy :: (Pos, Pos) -> SuperBoard -> XO -> IO (Pos, Pos)
randomStrategy _ b _ = selectRandom (emptySuperPositions b) 

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

-- finds the first valid subboard and do minimax
ai :: Player 
ai = Player "machine" aiStrategy

aiStrategy :: (Pos, Pos) -> SuperBoard -> XO -> IO (Pos, Pos)
aiStrategy _ superBoard turn = 
  return (try [Pos a b | a<-[1..3], b<-[1..3]])
    where
      try (superPos:ps) = 
        case minimax turn turn (fromJust (M.lookup superPos superBoard)) of
          (Nothing, _) -> try ps
          (Just pos, _)  -> (superPos, pos)
      try [] = (Pos 0 0, Pos 0 0)

testboard1 = M.fromList [(Pos 1 1, O), (Pos 1 3, X), (Pos 2 1, X), (Pos 3 1, X), (Pos 3 2, O), (Pos 3 3, O)]
testboard2 = M.fromList [(Pos 1 2, X), (Pos 2 3, X), (Pos 3 1, O), (Pos 3 2, O), (Pos 3 3, X)]

getMoves board = [Pos a b | a<-[1..3], b<-[1..3], canPut a b]
  where 
    canPut a b = isNothing (M.lookup (Pos a b) board)

optimizer me turn = L.foldl1' opt --base
  where 
    factor = if me == turn then 1.0 else -1.0
    opt (bestmove, bestscore) (move, score) = 
      if (score - bestscore) * factor > 0 
        then (move, score)
        else (bestmove, bestscore)

minimax me turn board =
  case Model.Board.getBoardResult board of
    Draw       -> (Nothing, 0.0)
    Win winner -> (Nothing, if winner == me then 1.0 else -1.0)
    Retry      -> (Nothing, 0.0)
    _          -> 
      let
        movesAndScores  = [(move, snd (minimax me (flipXO turn) (M.insert move turn board))) | move <- getMoves board]
        (bestmove, bestscore) = optimizer me turn movesAndScores
      in
        (Just bestmove, 0.8 * bestscore)

