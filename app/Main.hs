module Main
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Reversi.State                 as S

main :: IO ()
main = do
    S.printNicely S.initial
    mapM_ S.printNicely (getNext S.initial)
