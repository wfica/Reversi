--{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.RandomPlay
where

import           Reversi
import qualified Reversi.State                 as S

import           System.Random


infixl 9 !#! -- takes n-th element of a list 
(!#!) :: [a] -> Int -> Maybe a
[] !#! _         = Nothing
xs !#! n | n < 0 = Nothing
(x : _ ) !#! 0   = Just x
(_ : xs) !#! n   = xs !#! (n - 1)

makeRandomMove :: State -> IO (Maybe State)
makeRandomMove state = (!#!) nexts <$> randomRIO (0, len - 1)
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
