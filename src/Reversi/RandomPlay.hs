--{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.RandomPlay
where

import           Reversi
import qualified Reversi.State                 as S

import           System.Random


makeRandomMove :: State -> IO (Maybe State)
makeRandomMove state = if len == 0
    then return Nothing
    else Just . (!!) nexts <$> randomRIO (0, len - 1)
  where
    nexts = getNext state
    len   = length nexts

randomGame :: IO State
randomGame = go S.initial
  where
    go :: State -> IO State
    go s = do
        maybeNext <- makeRandomMove s
        case maybeNext of
            Nothing   -> return s
            Just next -> go next
