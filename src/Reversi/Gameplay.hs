--{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.Gameplay
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Reversi.State                 as S
import qualified Data.Vector                   as V
import qualified Reversi.AgentClever           as AC
import qualified Reversi.AgentRandom           as AR


play :: State -> (Int, Int)
play s = case player of
    X -> case AR.chooseMove moves of
        Nothing -> S.getStats s
        Just c  -> play (moves !! c)
    O -> case AC.chooseMove moves of
        Nothing -> S.getStats s
        Just c  -> play (moves !! c)
  where
    player = getPlayer s
    moves  = getNext s


playOneGame :: (Int, Int)
playOneGame = play S.initial

twoToThree :: (a, b) -> c -> (a, b, c)
twoToThree (x, y) z = (x, y, z)

playWithLog :: State -> [State]-> (Int, Int, [State])
playWithLog s log = case player of
    X -> case AR.chooseMove moves of
        Nothing -> twoToThree (S.getStats s) log 
        Just c  -> playWithLog (moves !! c) (s:log)
    O -> case AC.chooseMove moves of
        Nothing -> twoToThree (S.getStats s) log 
        Just c  -> playWithLog (moves !! c) (s:log)
  where
    player = getPlayer s
    moves  = getNext s

playOneGameWithLog :: (Int, Int, [State])
playOneGameWithLog = playWithLog S.initial [] 

