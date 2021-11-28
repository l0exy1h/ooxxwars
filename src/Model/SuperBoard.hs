module Model.SuperBoard where

import Model.Board
import qualified Data.Map as M 

type SuperBoard = M.Map Pos Board

superBoardInit :: Int -> SuperBoard
superBoardInit x = M.fromList[(p, Model.Board.init) | p <- (superPositions x)]

superPositions :: Int -> [Pos]
superPositions d = [ Pos x y | x <- [1..d], y <- [1..d] ]

isqrt :: Int -> Int
isqrt x = floor . sqrt $ (fromIntegral x :: Float)

getSuperBoardDim :: SuperBoard -> Int
getSuperBoardDim sb = isqrt (M.size sb)

put :: SuperBoard -> XO -> Pos -> Pos -> Result SuperBoard
put sb xo supPos subPos = case M.lookup supPos sb of
    Nothing   -> Retry
    Just sub  -> case (Model.Board.put sub xo subPos) of 
        Retry   -> Retry
        _       -> result (M.insert supPos (M.insert subPos xo sub) sb)


winPositions :: Int -> [[Pos]]
winPositions x = (rows x) ++ (cols x) ++ (diags x) 

rows, cols, diags :: Int -> [[Pos]]
rows x = [[Pos r c | c <- [1..x]] | r <- [1..x]]
cols x = [[Pos r c | r <- [1..x]] | c <- [1..x]]
diags x = [[Pos i i | i <- [1..x]], [Pos i (x+1-i) | i <- [1..x]]]

result :: SuperBoard -> Result SuperBoard
result sb   | wins sb X = Win X
            | wins sb O = Win O
            | isDraw sb = Draw
            | otherwise = Cont sb

isDraw :: SuperBoard -> Bool
isDraw sb = and[isFull xx | xx <- M.elems sb]

wins :: SuperBoard -> XO -> Bool
wins sb xo = or [ winsPoss sb xo ps | ps <- (winPositions (getSuperBoardDim sb))]


winsPoss :: SuperBoard -> XO -> [Pos] -> Bool
winsPoss sb xo ps = and [ checkWinOfBoard sb xo p | p <- ps ]

checkWinOfBoard :: SuperBoard -> XO -> Pos -> Bool
checkWinOfBoard sb xo ps = case M.lookup ps sb of
    Nothing -> False
    Just v  -> if getBoardResult v == Win xo then True else False


superUp :: Int -> (Pos, Pos) -> (Pos, Pos) 
superUp _ posPair = case pRow (snd posPair) of
    0   -> (case pRow (fst posPair) of
            0   ->  posPair
            x   ->  ((fst posPair){pRow = x - 1}, (snd posPair){pRow = 3}) 
        )
    _   -> (fst posPair, Model.Board.up (snd posPair))

superDown :: Int -> (Pos, Pos) -> (Pos, Pos) 
superDown sbDim posPair = case pRow (snd posPair) of
    3   -> if pRow (fst posPair) == sbDim 
        then posPair 
        else((fst posPair){pRow = (pRow (fst posPair)) + 1}, (snd posPair){pRow = 1}) 
    _     -> (fst posPair, Model.Board.down (snd posPair))



superLeft :: Int -> (Pos, Pos) -> (Pos, Pos) 
superLeft _ posPair = case pCol (snd posPair) of
    0   -> (case pCol (fst posPair) of
            0   ->  posPair
            x   ->  ((fst posPair){pCol = x - 1}, (snd posPair){pCol = x - 1}) 
          )
    _     -> (fst posPair, Model.Board.left (snd posPair))

superRight :: Int -> (Pos, Pos) -> (Pos, Pos) 
superRight sbDim posPair = case pCol (snd posPair) of
    3   -> if (pCol (fst posPair)) == sbDim 
        then posPair 
        else ((fst posPair){pCol = (pCol (fst posPair)) + 1}, (snd posPair){pCol = 1}) 
           
    _     -> (fst posPair, Model.Board.right (snd posPair))

--- >>> fst (superRight 3 (Pos 2 1, Pos 1 3))
--- Pos {pRow = 2, pCol = 2}
---

--- >>> fst (superDown 3 (Pos 1 1, Pos 3 3))
--- Pos {pRow = 2, pCol = 1}
---
