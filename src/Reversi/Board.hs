{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Reversi.Board
where

import           Reversi
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as GM

empty :: Board
empty = G.create $ do
    vec <- GM.new 64
    GM.set vec Nothing
    GM.write vec (positionToIndex (3, 4)) (Just O)
    GM.write vec (positionToIndex (4, 3)) (Just O)
    GM.write vec (positionToIndex (3, 3)) (Just X)
    GM.write vec (positionToIndex (4, 4)) (Just X)
    return vec

printNicely :: Board -> IO ()
printNicely  = V.ifoldM_ (\ _ i elem -> printIt elem >> printNewLine i ) () 
    where
        printIt Nothing = putChar '_'
        printIt (Just x) = putStr $ show x
        printNewLine i = if i `mod` 8 == 7 then putChar '\n' else putStr ""

printNicelyLn :: Board -> IO ()
printNicelyLn board = printNicely board >> putChar '\n' 




