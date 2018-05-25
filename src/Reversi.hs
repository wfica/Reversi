module Reversi where

import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
data Player = X | O deriving (Show, Eq)
type Trio = Maybe Player
type Position = (Int, Int)

type Board = V.Vector Trio
-- getPlayer returns a player who has current move
data State = State {
    getBoard :: Board,
    getPlayer :: Player,
    getPreviousMove :: Maybe Int,
    getNext :: [State]
}    
    
positionToIndex :: Position -> Int
positionToIndex (x, y) = 8 * x + y

theOtherPlayer :: Player -> Player
theOtherPlayer X = O
theOtherPlayer O = X

jumps :: [(Int, Int)]
jumps = [(1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1)]

indexJumps :: [Int]
indexJumps = map positionToIndex jumps