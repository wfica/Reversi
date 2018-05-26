--{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.State
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as GM

import           Data.Maybe                     ( catMaybes )


initial :: State
initial = State B.empty O Nothing (generateMoves initial)

properDirection
    :: Board -> Index -> Int -> Player -> [(Index, Field)] -> [(Index, Field)]
properDirection board i jump turn acc = if outOfRangeIndex i
    then []
    else case board V.! i of
        Nothing -> []
        Just turn2 | turn2 == turn -> acc
        _ -> properDirection board (i + jump) jump turn ((i, Just turn) : acc)


makeMove :: Index -> Field -> Player -> Board -> Maybe State
makeMove _ (Just _) _    _     = Nothing
makeMove i Nothing  turn board = case changes of
    [] -> Nothing
    _  -> Just nextBoard
  where
    changes :: [(Index, Field)]
    changes = do
        jump <- indexJumps
        properDirection board (i + jump) jump turn []
    nextBoard :: State 
    nextBoard = State (board V.// ((i, Just turn) : changes))
                      (theOtherPlayer turn)
                      (Just i)
                      (generateMoves nextBoard)


generateMoves :: State -> [State]
generateMoves state = catMaybes
    $ V.ifoldl (\acc i elem -> makeMove i elem turn board : acc) [] board
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


