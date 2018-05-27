module Reversi
where

import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
data Player = X | O deriving (Show, Eq)
type Field = Maybe Player
type Index = Int
type Position = (Index, Index)
type Jump = (Int, Int)
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

indexToPosition :: Index -> Position
indexToPosition x = (x `div` 8, x `mod` 8)

theOtherPlayer :: Player -> Player
theOtherPlayer X = O
theOtherPlayer O = X


jumps :: [Jump]
jumps = [(1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1)]

jump :: Position -> Jump -> Position
jump (x, y) (a, b) = (x + a, y + b)


indexJumps :: [Int]
indexJumps = map positionToIndex jumps

outOfRangePosition :: Position -> Bool
outOfRangePosition (x, y) = x < 0 || x >= 8 || y < 0 || y >= 8

outOfRangeIndex :: Index -> Bool
outOfRangeIndex i = i < 0 || i >= 64
