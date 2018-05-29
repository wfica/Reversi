module Main
where

import qualified Reversi.State                 as S
import           Reversi.RandomPlay

main :: IO ()
main = randomGame >>= S.printNicely

