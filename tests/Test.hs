{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where 
import Data.IORef
import System.Exit
import System.IO
import Model.SuperBoard
import Model.Board
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M 
import Prelude hiding (maximum)

type Score = IORef (Int, Int)

runTests :: [Score -> TestTree] -> IO ()
runTests groups = do
  sc <- initScore
  defaultMain (localOption (mkTimeout 2000000) (tests sc groups)) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- readIORef sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> [Score -> TestTree] -> TestTree
tests x gs = testGroup "Tests" [ g x | g <- gs ]

main :: IO ()
main = runTests 
  [ probMove,
    probWin
  ]

-- >>> main
-- Tests
--   SuperBoard-Move
--     up-1:    OK
--     up-2:    OK
--     up-3:    OK
--     down-1:  OK
--     down-2:  OK
--     down-3:  OK
--     left-1:  OK
--     left-2:  OK
--     left-3:  OK
--     right-1: OK
--     right-2: OK
--     right-3: OK
--   SuperBoard-Win
--     Win-X-1: OK
-- <BLANKLINE>
-- All 13 tests passed (0.00s)
-- OVERALL SCORE = 13 / 13
-- *** Exception: ExitSuccess
--

probMove ::  Score -> TestTree
probMove sc = testGroup "SuperBoard-Move" [
  scoreTest ((\_ -> superUp (Pos 1 1, Pos 1 1)), (), (Pos 1 1, Pos 1 1), 1, "up-1"),
  scoreTest ((\_ -> superUp (Pos 2 1, Pos 1 1)), (), (Pos 1 1, Pos 3 1), 1, "up-2"),
  scoreTest ((\_ -> superUp (Pos 2 1, Pos 2 1)), (), (Pos 2 1, Pos 1 1), 1, "up-3"),
  scoreTest ((\_ -> superDown (Pos 3 1, Pos 3 1)), (), (Pos 3 1, Pos 3 1), 1, "down-1"),
  scoreTest ((\_ -> superDown (Pos 1 1, Pos 3 1)), (), (Pos 2 1, Pos 1 1), 1, "down-2"),
  scoreTest ((\_ -> superDown (Pos 1 1, Pos 1 1)), (), (Pos 1 1, Pos 2 1), 1, "down-3"),
  scoreTest ((\_ -> superLeft (Pos 1 1, Pos 1 1)), (), (Pos 1 1, Pos 1 1), 1, "left-1"),
  scoreTest ((\_ -> superLeft (Pos 1 2, Pos 1 1)), (), (Pos 1 1, Pos 1 3), 1, "left-2"),
  scoreTest ((\_ -> superLeft (Pos 1 1, Pos 1 2)), (), (Pos 1 1, Pos 1 1), 1, "left-3"),
  scoreTest ((\_ -> superRight (Pos 1 3, Pos 1 3)), (), (Pos 1 3, Pos 1 3), 1, "right-1"),
  scoreTest ((\_ -> superRight (Pos 1 1, Pos 1 3)), (), (Pos 1 2, Pos 1 1), 1, "right-2"),
  scoreTest ((\_ -> superRight (Pos 1 1, Pos 1 1)), (), (Pos 1 1, Pos 1 2), 1, "right-3")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)


inputFlatten :: [String] -> [(Int, Int, Char)]
inputFlatten x = [(row, col, ch)| (row, str) <- zip [0..8] x, (col, ch) <- zip [0..8] str]

indicesToPosPos :: (Int, Int) -> (Pos, Pos)
indicesToPosPos (row, col) = (Pos supRow supCol, Pos subRow subCol)
                            where
                              supRow = (row `div` 3) + 1
                              supCol = (col `div` 3) + 1
                              subRow = (row `mod` 3) + 1
                              subCol = (col `mod` 3) + 1


superInsert :: SuperBoard -> XO -> (Pos, Pos) -> SuperBoard
superInsert sb xo (supPos, subPos) = case M.lookup supPos sb of
    Nothing   -> sb
    Just sub  -> (M.insert supPos (M.insert subPos xo sub) sb)

superBoardConstructor :: [[Char]] -> SuperBoard
superBoardConstructor ss = foldr f base (inputFlatten ss)
                          where 
                            base = superBoardInit 3
                            f (row, col, ch) sb | ch == 'X' = superInsert sb X (indicesToPosPos (row, col))| ch == 'O' = superInsert sb O (indicesToPosPos (row, col))| otherwise = sb  

probWin ::  Score -> TestTree
probWin sc = testGroup "SuperBoard-Win" [
  scoreTest ((\_ -> superResult (superBoardConstructor ["XOOOXOOOX","XOOOXOOOX","XOOOXOOOX","XOOOXOOOX","XOOOXOOOX","XOOOXOOOX","XOOOXOOOX","XOOOXOOOX","XOOOXOOOX"])), (), Win X, 1, "Win-X-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)



--------------------------------------------------------------------------------
-- | Construct a single test case
--------------------------------------------------------------------------------
mkTest' :: (Show b, Eq b) => Score -> (a -> IO b) -> a -> b -> String -> TestTree
--------------------------------------------------------------------------------
mkTest' sc f x r name = scoreTest' sc (f, x, r, 1, name)

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> IO b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    actR <- f x
    if actR == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)