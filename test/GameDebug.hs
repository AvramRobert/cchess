module GameDebug where

import Criterion.Main
import Lib.Coll
import Data.List
import Data.Maybe
import qualified Chess.Internal as CI
import qualified PGN.Internal as PI
import qualified PGN as P
import System.IO.Unsafe (unsafePerformIO)

pgnGames = unsafePerformIO $ P.fromFile' "./test/resources/games/carlsen.pgn"

testGame  = "[Event \"Bygger'n Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2005.10.01\"]\r[Round \"1\"]\r[White \"Svensen,T\"]\r[Black \"Carlsen,M\"]\r[Result \"0-1\"]\r[WhiteElo \"2112\"]\r[BlackElo \"2570\"]\r[ECO \"D00\"]\r\r1.d4 Nf6 2.Bg5 d5 3.Bxf6 exf6 4.e3 c6 5.Bd3 Bd6 6.Qf3 Qb6 7.b3 h5 8.h3 Rh6\r9.g3 Be6 10.Ne2 Nd7 11.Nd2 O-O-O 12.c3 f5 13.h4 g6 14.b4 Nf6 15.Nb3 Ne4 16.O-O-O f4\r17.Bxe4 dxe4 18.Qxe4 Bf5 19.Qg2 fxe3 20.fxe3 Re8 21.Qf2 Qa6 22.Kb2 Rhh8 23.Rhe1 b6\r24.a3 Re7 25.Nf4 Rhe8 26.Nxh5 Qc4 27.Ng7 Be6 28.Nxe6 Rxe6 29.Qf3 Qd5 30.Qxd5 cxd5\r31.g4 Re4 32.c4 dxc4 33.Nd2 Rxg4 34.Nxc4 Bc7 35.Kc3 Rxh4 36.d5 b5 37.Nb2 Bb6\r38.Rd3 Kd7 39.Re2 Kd6 40.a4 Rc8+ 41.Kb3 Rcc4 42.e4 Rcxe4 43.Rxe4 Rxe4 44.axb5 Rd4\r45.Nc4+ Kxd5 46.Nxb6+ axb6 47.Rc3 Ke5 48.Rc6 Rd6  0-1\r\r"

testGame'  = "[Event \"Bygger'n Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2005.10.01\"]\r[Round \"1\"]\r[White \"Svensen,T\"]\r[Black \"Carlsen,M\"]\r[Result \"0-1\"]\r[WhiteElo \"2112\"]\r[BlackElo \"2570\"]\r[ECO \"D00\"]\r\r1.d4 Nf6 1-0" --2.Bg5 d5 3.Bxf6 exf6 4.e3 c6 5.Bd3 Bd6 6.Qf3 Qb6 7.b3 h5 8.h3 Rh6\r9.g3 Be6 10.Ne2 Nd7 11.Nd2 1-0"-- O-O-O 12.c3 f5 13.h4 g6 14.b4 Nf6 15.Nb3 Ne4 16.O-O-O f4\r17.Bxe4 dxe4 18.Qxe4 Bf5 19.Qg2 fxe3 20.fxe3 Re8 21.Qf2 Qa6 22.Kb2 Rhh8 23.Rhe1 b6\r24.a3 Re7 25.Nf4 Rhe8 26.Nxh5 Qc4 27.Ng7 Be6 28.Nxe6 Rxe6 29.Qf3 Qd5 30.Qxd5 cxd5\r31.g4 Re4 32.c4 dxc4 33.Nd2 Rxg4 34.Nxc4 Bc7 35.Kc3 Rxh4 36.d5 b5 37.Nb2 Bb6\r38.Rd3 Kd7 39.Re2 Kd6 40.a4 Rc8+ 41.Kb3 Rcc4 42.e4 Rcxe4 43.Rxe4 Rxe4 44.axb5 Rd4\r45.Nc4+ Kxd5 46.Nxb6+ axb6 47.Rc3 Ke5 48.Rc6 Rd6  0-1\r\r"

testBoard = case (PI.parseCompute testGame) of (Right b) -> b; (Left e) -> CI.board

faultyMove = CI.Advance (CI.Pos CI.Rook CI.B (7, 6)) (7, 8)

faultyBoard = CI.apply testBoard faultyMove