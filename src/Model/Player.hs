module Model.Player where

import Model.Board
import Model.SuperBoard
import System.Random -- (Random(randomRIO))

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

-- rando :: Player 
-- rando = Player "machine" randomStrategy

randomStrategy :: a -> SuperBoard -> b -> IO (Pos, Pos)
randomStrategy _ b _ = selectRandom (emptySuperPositions b) 

-- randomStrategy :: a -> Board -> b -> IO Pos
-- randomStrategy _ b _ = selectRandom (emptyPositions b) 

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)