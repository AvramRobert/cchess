module Main where

import Criterion.Main (defaultMain, bgroup, bench, whnf)
import qualified Chess.Internal as CI
import qualified PGN.Internal as PI
import qualified PGN as P
import System.IO.Unsafe (unsafePerformIO)

pgnGames = unsafePerformIO $ P.fromFile' "./test/resources/games/carlsen.pgn"
testGame  = "[Event \"Open NOR-ch\"]\r[Site \"Oslo NOR\"]\r[Date \"2001.04.10\"]\r[Round \"5\"]\r[White \"Kabashaj,Agron\"]\r[Black \"Carlsen,M\"]\r[Result \"0-1\"]\r[WhiteElo \"\"]\r[BlackElo \"2064\"]\r[ECO \"A46\"]\r\r1.d4 Nf6 2.Nf3 c5 3.g3 cxd4 4.Nxd4 d5 5.Nf3 Nc6 6.c4 e5 7.cxd5 Nxd5 8.e4 Ndb4\r9.Qxd8+ Kxd8 10.Na3 Bg4 11.Be2 Bxf3 12.Bxf3 Nd4 13.Bd1 Nd3+ 14.Kf1 Rc8 15.Be3 Nxb2\r16.Bxd4 exd4 17.Nb5 Bc5 18.e5 a6 19.Nd6 Bxd6 20.exd6 d3 21.Bb3 Re8 22.Kg2 Re2\r23.Rab1 Rc6 24.d7 Kxd7 25.Bxf7 Rf6 26.Bd5 Rfxf2+ 27.Kh3 b5 28.g4 Re3+ 29.Kh4 g5+\r30.Kh5 Rh3+ 31.Kxg5 Rhxh2 32.Rhg1 Re2 33.Bb7 Re6 34.Bg2 Nc4 35.Rbd1 d2 36.Bd5 Re5+\r37.Kf6 Rxd5 38.Kg7 Nb2 39.Rh1 Rxh1 40.Rxh1 d1=Q 41.Rxd1 Nxd1 42.Kxh7 Rg5  0-1\r\r"

parseTestBoard game = case (PI.parseCompute game) of (Right b) -> b; (Left e) -> CI.board

benchMain :: IO ()
benchMain = defaultMain [ bgroup "API" [ bench "game1" $ whnf parseTestBoard $ (pgnGames !! 0),
                                         bench "game2" $ whnf parseTestBoard $ (pgnGames !! 1),
                                         bench "game3" $ whnf parseTestBoard $ (pgnGames !! 2),
                                         bench "standard-game" $ whnf parseTestBoard testGame ] ]

main :: IO ()
main = benchMain