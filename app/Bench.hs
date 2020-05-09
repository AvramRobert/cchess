module Bench (runBench) where

import Criterion.Main (defaultMain, bgroup, bench, whnf)
import qualified Chess.Game as G
import qualified Chess as C
import qualified PGN.Writer as W
import System.IO.Unsafe (unsafePerformIO)

pgnGames = unsafePerformIO $ C.pgnFromFile "./test/resources/games/carlsen.pgn"
newGame  = C.newGame "" ""

parseBoard game = case (C.parseGame game) of 
    (Right game) -> G.board game 
    (Left e)     -> G.board newGame

boards = [ parseBoard (pgnGames !! 0),
           parseBoard (pgnGames !! 1),
           parseBoard (pgnGames !! 2),
           parseBoard (pgnGames !! 3) ]

runBench :: IO ()
runBench = defaultMain [ bgroup "Parser" [ bench "game1" $ whnf parseBoard $ (pgnGames !! 0),
                                           bench "game2" $ whnf parseBoard $ (pgnGames !! 1),
                                           bench "game3" $ whnf parseBoard $ (pgnGames !! 2),
                                           bench "game4" $ whnf parseBoard $ (pgnGames !! 3) ],
                                            
                         bgroup "Writer" [ bench "game1" $ whnf W.writeMoves $ (boards !! 0), 
                                           bench "game2" $ whnf W.writeMoves $ (boards !! 1), 
                                           bench "game3" $ whnf W.writeMoves $ (boards !! 2),
                                           bench "game4" $ whnf W.writeMoves $ (boards !! 3) ]]