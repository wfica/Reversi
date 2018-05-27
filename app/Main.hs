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
    where  
        (a, b, log) = playOneGameWithLog


-- main = do 
--     mapM_ S.printNicely (S.getIthGeneration S.initialTest 2)

