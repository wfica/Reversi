module Main
where

import           Reversi
import qualified Reversi.Board                 as B
import qualified Reversi.State                 as S
import           Reversi.Gameplay

main :: IO ()
main = do 
    print a
    print b
    mapM_ S.printNicely log 
    print "-----------"
    mapM_ S.printNicely (S.getIthGeneration stateTest 0)
    print "-----------"
    mapM_ S.printNicely (S.getIthGeneration stateTest 1)
    where  
        (a, b, log) = playOneGameWithLog



boardTest = B.strToBoard "__________________________OOO______OOO______OOOX___OXXXX__OXXXXX"
stateTest = State boardTest X Nothing (S.generateMoves stateTest)

