module Main
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Reversi.State                 as S

main :: IO ()
main = do
    print "0"
    S.printNicely S.initial
    print "1"
    mapM_ S.printNicely (S.getIthGeneration S.initial 1)
    print "2"
    mapM_ S.printNicely (S.getIthGeneration S.initial 2)

