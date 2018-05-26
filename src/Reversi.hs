module Reversi where

import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
data Player = X | O deriving (Show, Eq)
type Field = Maybe Player
type Index = Int
type Position = (Index, Index)
type Board = V.Vector Field
-- getPlayer returns a player who has current move
data State = State {
    getBoard :: Board,
    getPlayer :: Player,
    getPreviousMove :: Maybe Index,
    getNext :: [State]
}    
    
positionToIndex :: Position -> Index
positionToIndex (x, y) = 8 * x + y

theOtherPlayer :: Player -> Player
theOtherPlayer X = O
theOtherPlayer O = X

jumps :: [(Int, Int)]
jumps = [(1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1)]

indexJumps :: [Int]
indexJumps = map positionToIndex jumps

outOfRangeIndex :: Index -> Bool
outOfRangeIndex i = i < 0 || i >= 64