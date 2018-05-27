--{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.AgentRandom
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Reversi.State                 as S
import qualified Data.Vector                   as V

chooseMove :: [State] -> Maybe Int 
chooseMove [] = Nothing 
chooseMove xs = Just 0 
