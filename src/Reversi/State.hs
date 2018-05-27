--{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.State
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Data.Vector                   as V

import           Data.Maybe                     ( catMaybes )


initial :: State
initial = State B.empty O Nothing (generateMoves initial)

properDirection
  :: Board -> Position -> Jump -> Player -> [(Index, Field)] -> [(Index, Field)]
properDirection board pos dir turn acc = if outOfRangePosition pos
  then []
  else case board V.! i of
    Nothing -> []
    Just turn2 | turn2 == turn -> acc
    _ -> properDirection board (jump pos dir) dir turn ((i, Just turn) : acc)
  where i = positionToIndex pos


makeMove :: Position -> Field -> Player -> Board -> Maybe State
makeMove _   (Just _) _    _     = Nothing
makeMove pos Nothing  turn board = case changes of
  [] -> Nothing
  _  -> Just nextBoard
 where
  i :: Index
  i = positionToIndex pos
  changes :: [(Index, Field)]
  changes = do
    dir <- jumps
    properDirection board (jump pos dir) dir turn []
  nextBoard :: State
  nextBoard = State (board V.// ((i, Just turn) : changes))
                    (theOtherPlayer turn)
                    (Just i)
                    (generateMoves nextBoard)


generateMoves :: State -> [State]
generateMoves state = catMaybes $ V.ifoldl
  (\acc i elem -> makeMove (indexToPosition i) elem turn board : acc)
  []
  board
-- $ V.ifoldl (\acc i elem -> (makeMove i elem <$> getPlayer <*> getBoard) state : acc) [] board
 where
  turn  = getPlayer state
  board = getBoard state


printNicely :: State -> IO ()
printNicely state = do
  putStrLn $ "player: " ++ show (getPlayer state)
  B.printNicely (getBoard state)

getIthGeneration :: State -> Int -> [State]
getIthGeneration s 0 = [s]
getIthGeneration s n = do
  ns <- getNext s
  getIthGeneration ns (n - 1)

getStats :: State -> (Int, Int)
getStats s = foldl add (0, 0) (getBoard s)
 where
  add (x, o) Nothing  = (x, o)
  add (x, o) (Just X) = (x + 1, o)
  add (x, o) (Just O) = (x, o + 1)


initialTest :: State
initialTest = State B.emptyTest O Nothing (generateMoves initial)