{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

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
    :: Board -> Int -> Int -> Player -> [(Int, Trio)] -> [(Int, Trio)]
properDirection board i jump turn acc =
    if board V.! i /= Just (theOtherPlayer turn)
        then acc
        else properDirection board (i + jump) jump turn ((i, Just turn) : acc)


makeMove :: Int -> Trio -> Player -> Board -> Maybe State
makeMove _ Nothing  _ _ = Nothing
makeMove _ (Just X) O _ = Nothing
makeMove _ (Just O) X _ = Nothing
makeMove i (Just s) turn board =
    let changes = concatMap
            (\jump -> properDirection board (i + jump) jump turn [])
            indexJumps
    in  if null changes
            then Nothing
            else
                let nextBoard = State (board V.// changes)
                                      (theOtherPlayer turn)
                                      (Just i)
                                      (generateMoves nextBoard)
                in  Just nextBoard



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



