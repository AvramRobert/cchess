module GameDebug where

import Lib.Coll
import Data.List
import Data.Maybe
import Chess.Internal2
import PGN.Internal2    

testGame  = "1.e4 c5 2.Nf3 d6 3.d4 cxd4 4.Nxd4 Nf6 5.Nc3 Nc6 6.Bg5 e5 7.Bxf6 gxf6 8.Nf5 Bxf5\r9.exf5 Be7 10.Nd5 O-O 11.c3 Kh8 12.Bd3 Nb8 13.O-O Nd7 14.Be4 Nb6 15.Nxb6 Qxb6\r16.Qb3 Qc7 17.Bd5 Bd8 18.Rad1 Qd7 19.Rd3 Qxf5 20.Rf3 Qg6 21.Qxb7 Bb6 22.Be4 Qg4\r23.Bxh7 Kxh7 24.Rh3+ Qxh3 25.gxh3 f5 26.Kh1 Rae8 27.Rg1 Bxf2 28.Rg2 Be3 29.Qf3 Bh6\r30.Qxf5+ Kh8 31.Qf6+ Kh7 32.Qxd6 e4 33.Rg4 Re6 34.Qc5 e3 35.Qf5+ Kh8 36.Re4 Rg6\r37.Rg4 Re6 38.Re4 Rg6 39.Rh4 Kg7 40.Qe5+ Kg8 41.Rg4 Kh7 42.Qe4 Rg8 43.Qf5 Rf8\r44.Kg2 Kg7 45.Kf3 Kh8 46.Qe5+ Kh7 47.Qe4 Kh8 48.Ke2 Re6 49.Qd4+ Kh7 50.Qxa7 Rf6\r51.Qd4 Rf2+ 52.Ke1 Re8 53.Qd3+ Kh8 54.Qd6 Kh7 55.Rh4 Re6 56.Qd3+ Kh8 57.Qd4+ Kh7\r58.Qd3+ Kh8 59.Rxh6+ Rxh6 60.Qxe3 Rff6 61.Qd4 Kg8 62.a4 Re6+ 63.Kf2 Ref6+\r64.Kg3 Rhg6+ 65.Kh4 Rd6 66.Qe4 Rde6 67.Qa8+ Kg7 68.Qd5 Re2 69.Qd4+ Kh7 70.Qf4 Kg8\r71.Qb8+ Kg7 72.Qf4 Reg2 73.Qd4+ Kh7 74.Qe4 Rxb2 75.a5 Rb8 76.a6 Rh8 77.Qe5 Kg8+  0-1\r\r"

testGame' = "1.e4 c5 2.Nf3 d6 3.d4 cxd4 4.Nxd4 Nf6 5.Nc3 Nc6 6.Bg5 e5 7.Bxf6 gxf6 8.Nf5 Bxf5\r9.exf5 Be7 1-0 "-- 10.Nd5 O-O 11.c3 Kh8 12.Bd3 Nb8 13.O-O Nd7 14.Be4 Nb6 15.Nxb6 Qxb6\r16.Qb3 Qc7 17.Bd5 Bd8 18.Rad1 Qd7 19.Rd3 Qxf5 20.Rf3 Qg6 21.Qxb7 Bb6 22.Be4 Qg4\r23.Bxh7 Kxh7 24.Rh3+ Qxh3 25.gxh3 f5 26.Kh1 Rae8 27.Rg1 Bxf2 28.Rg2 Be3 29.Qf3 Bh6\r30.Qxf5+ Kh8 31.Qf6+ Kh7 32.Qxd6 e4 33.Rg4 Re6 34.Qc5 e3 35.Qf5+ Kh8 36.Re4 Rg6 \r37.Rg4 Re6 38.Re4 Rg6 39.Rh4 Kg7 40.Qe5+ Kg8 41.Rg4 Kh7 42.Qe4 1-0 " --Rg8 1-0 " -- 43.Qf5 1-0  Rf8 1-0 " -- \r44.Kg2 Kg7 45.Kf3 Kh8 46.Qe5+ Kh7 47.Qe4 Kh8 48.Ke2 1-0 "-- Re6 49.Qd4+ Kh7 50.Qxa7 Rf6\r51.Qd4 Rf2+ 52.Ke1 1-0"--Re8 53.Qd3+ Kh8 54.Qd6 Kh7 55.Rh4 Re6 56.Qd3+ Kh8 57.Qd4+ Kh7\r58.Qd3+ Kh8 59.Rxh6+ Rxh6 60.Qxe3 Rff6 61.Qd4 Kg8 62.a4 Re6+ 63.Kf2 Ref6+\r64.Kg3 Rhg6+ 65.Kh4 Rd6 66.Qe4 Rde6 67.Qa8+ Kg7 68.Qd5 Re2 69.Qd4+ Kh7 70.Qf4 Kg8\r71.Qb8+ Kg7 1-0"--72.Qf4 1-0" -- Reg2 73.Qd4+ Kh7 74.Qe4 Rxb2 75.a5 Rb8 76.a6 Rh8 77.Qe5 Kg8+  0-1\r\r"

-- In this case, both rooks can move to G8 apparently and my api picks the wrong one

testBoard = case (run turnMoveParser testGame') of (Right b) -> b; (Left e) -> []

faultyMove = Advance (Pos Rook B (7, 6)) (7, 8)

--faultyBoard = apply testBoard faultyMove

kingFor c = fromJust . find (every [(== King) . piece, (== c) . colour]) . pieces