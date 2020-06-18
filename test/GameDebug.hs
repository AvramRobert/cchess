module GameDebug where

import Criterion.Main
import Lib.Coll
import Data.List
import Data.Maybe
import qualified Chess.Internal as C
import qualified Chess.Game as G
import qualified PGN.Writer as W
import qualified Chess as C
import System.IO.Unsafe (unsafePerformIO)

pgnGames = unsafePerformIO $ C.pgnFromFile "./test/resources/games/carlsen.pgn"

-- game with fucked up header ordering
testGame = "[Event \"World Championship Tournament\"]\r\n[Site \"Mexico City MEX\"]\r\n[Date \"2007.09.15\"]\r\n[EventDate \"?\"]\r\n[Round \"3\"]\r\n[Result \"1/2-1/2\"]\r\n[White \"Viswanathan Anand\"]\r\n[Black \"Vladimir Kramnik\"]\r\n[ECO \"C42\"]\r\n[WhiteElo \"2803\"]\r\n[BlackElo \"2811\"]\r\n[PlyCount \"130\"]\r\n\r\n1.e4 e5 2.Nf3 Nf6 3.Nxe5 d6 4.Nf3 Nxe4 5.d4 d5 6.Bd3 Nc6 7.O-O\nBe7 8.c4 Nb4 9.Be2 O-O 10.Nc3 Bf5 11.a3 Nxc3 12.bxc3 Nc6\n13.Re1 Re8 14.cxd5 Qxd5 15.Bf4 Rac8 16.Qa4 Bd7 17.Qc2 Qf5\n18.Qxf5 Bxf5 19.Bb5 Bd7 20.d5 Ne5 21.Bxd7 Nxd7 22.Bxc7 Rxc7\n23.d6 Rxc3 24.dxe7 f6 25.Rad1 Rc7 26.Nd4 Ne5 27.f4 Nc6 28.Nxc6\nbxc6 29.Rd6 c5 30.Ree6 c4 31.Rc6 Rexe7 32.Rxc4 Rxc4 33.Rxe7\nRa4 34.Rb7 h6 35.f5 Rxa3 36.Kf2 h5 37.g3 a5 38.Ra7 a4 39.h4\nRa2+ 40.Kf3 a3 41.Ke3 Ra1 42.Kf2 Kf8 43.Kg2 a2 44.Kh2 Ke8\n45.Kg2 Kd8 46.Kh2 Kc8 47.Kg2 Kb8 48.Ra3 Kb7 49.Ra4 Kb6 50.Ra8\nKc5 51.Ra7 Kd5 52.Ra4 Ke5 53.Ra5+ Ke4 54.Kh2 Kf3 55.Ra3+ Kf2\n56.Ra4 Kf1 57.Kh1 Ke1 58.Kg2 Kd1 59.Ra7 Rc1 60.Rxa2 Rc2+\n61.Rxc2 Kxc2 62.Kf3 Kd3 63.g4 hxg4+ 64.Kxg4 Ke4 65.Kh5 Kxf5\n1/2-1/2\r\n"

testGame' = "[Event \"World Championship Tournament\"]\r\n[Site \"Mexico City MEX\"]\r\n[Date \"2007.09.15\"]\r\n[EventDate \"?\"]\r\n[Round \"3\"]\r\n[Result \"1/2-1/2\"]\r\n[White \"Viswanathan Anand\"]\r\n[Black \"Vladimir Kramnik\"]\r\n[ECO \"C42\"]\r\n[WhiteElo \"2803\"]\r\n[BlackElo \"2811\"]\r\n[PlyCount \"130\"]\r\n\r\n 1.e4 e5 2.Nf3 Nf6 3.Nxe5 d6 4.Nf3 Nxe4 5.d4 d5 6.Bd3 Nc6 7.O-O\nBe7 8.c4 Nb4 9.Be2 O-O 10.Nc3 Bf5 11.a3 Nxc3 12.bxc3 Nc6\n13.Re1 Re8 14.cxd5 Qxd5 15.Bf4 Rac8 16.Qa4 Bd7 17.Qc2 Qf5\n18.Qxf5 Bxf5 19.Bb5 Bd7 20.d5 Ne5 21.Bxd7 Nxd7 22.Bxc7 Rxc7\n23.d6 Rxc3 24.dxe7 f6 25.Rad1 Rc7 26.Nd4 Ne5 27.f4 Nc6 28.Nxc6\nbxc6 29.Rd6 c5 30.Ree6 c4 31.Rc6 Rexe7 32.Rxc4 Rxc4 33.Rxe7\nRa4 34.Rb7 h6 35.f5 Rxa3 36.Kf2 h5 37.g3 a5 38.Ra7 a4 39.h4\n 1-0" --Ra2+ 40.Kf3 a3 41.Ke3 Ra1 42.Kf2 Kf8 43.Kg2 a2 44.Kh2 Ke8\n45.Kg2 Kd8 46.Kh2 Kc8 47.Kg2 Kb8 48.Ra3 Kb7 49.Ra4 Kb6 50.Ra8\nKc5 51.Ra7 Kd5 52.Ra4 Ke5 53.Ra5+ Ke4 54.Kh2 Kf3 55.Ra3+ Kf2\n56.Ra4 Kf1 57.Kh1 Ke1 58.Kg2 Kd1 59.Ra7 Rc1 60.Rxa2 Rc2+\n61.Rxc2 Kxc2 62.Kf3 Kd3 63.g4 hxg4+ 64.Kxg4 Ke4 65.Kh5 Kxf5\n1/2-1/2\r\n"

testMoves = "1.e4 e5 2.Nf3 Nf6 3.Nxe5 d6 4.Nf3 Nxe4 5.d4 d5 6.Bd3 Nc6 7.O-O\nBe7 8.c4 Nb4 9.Be2 O-O 10.Nc3 Bf5 11.a3 Nxc3 12.bxc3 Nc6\n13.Re1 Re8 14.cxd5 Qxd5 15.Bf4 Rac8 16.Qa4 Bd7 17.Qc2 Qf5\n18.Qxf5 Bxf5 19.Bb5 Bd7 20.d5 Ne5 21.Bxd7 Nxd7 22.Bxc7 Rxc7\n23.d6 Rxc3 24.dxe7 f6 25.Rad1 Rc7 26.Nd4 Ne5 27.f4 Nc6 28.Nxc6\nbxc6 29.Rd6 c5 30.Ree6 c4 31.Rc6 Rexe7 32.Rxc4 Rxc4 33.Rxe7\nRa4 34.Rb7 h6 35.f5 Rxa3 36.Kf2 h5 37.g3 a5 38.Ra7 a4 39.h4\nRa2+ 40.Kf3 a3 41.Ke3 Ra1 42.Kf2 Kf8 43.Kg2 a2 44.Kh2 Ke8\n45.Kg2 Kd8 46.Kh2 Kc8 47.Kg2 Kb8 48.Ra3 Kb7 49.Ra4 Kb6 50.Ra8\nKc5 51.Ra7 Kd5 52.Ra4 Ke5 53.Ra5+ Ke4 54.Kh2 Kf3 55.Ra3+ Kf2\n56.Ra4 Kf1 57.Kh1 Ke1 58.Kg2 Kd1 59.Ra7 Rc1 60.Rxa2 Rc2+\n61.Rxc2 Kxc2 62.Kf3 Kd3 63.g4 hxg4+ 64.Kxg4 Ke4 65.Kh5 Kxf5\n1/2-1/2\r\n"

mvs = foldr (<>) "" $ intersperse " " $ lines testMoves

wholeGame = case (C.parseGame testGame) of
    (Right game) -> game
    (Left e)     -> C.quickGame

testBoard = G.gameBoard wholeGame

testPGN = foldr (<>) "" $ intersperse " " $ W.writeMoves testBoard

faultyMove = C.Advance (C.Pos C.Rook C.B (1, 3)) (1, 2)

faultyBoard = C.forceApply testBoard faultyMove