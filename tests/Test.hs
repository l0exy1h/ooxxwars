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
  [ probMove
  ]

-- >>> main
-- Tests
--   SuperBoard-Move
--     up-1: OK
--     up-2: OK
-- <BLANKLINE>
-- All 2 tests passed (0.00s)
-- OVERALL SCORE = 2 / 2
-- *** Exception: ExitSuccess
--

probMove ::  Score -> TestTree
probMove sc = testGroup "SuperBoard-Move" [
  scoreTest ((\_ -> superUp (Pos 1 1, Pos 1 1)), (), (Pos 1 1, Pos 1 1), 1, "up-1"),
  scoreTest ((\_ -> superUp (Pos 2 1, Pos 1 1)), (), (Pos 1 1, Pos 3 1), 1, "up-2")
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