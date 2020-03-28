module GameDebug where

import Lib.Coll
import Data.List
import Data.Maybe
import Chess.Internal2
import PGN.Internal2    

testGame  = "[Event \"Troll Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2001.01.05\"]\r[Round \"1\"]\r[White \"Edvardsen,R\"]\r[Black \"Carlsen,Magnus\"]\r[Result \"1/2-1/2\"]\r[WhiteElo \"2055\"]\r[BlackElo \"\"]\r[ECO \"D12\"]\r\r1.d4 Nf6 2.Nf3 d5 3.e3 Bf5 4.c4 c6 5.Nc3 e6 6.Bd3 Bxd3 7.Qxd3 Nbd7 8.b3 Bd6\r9.O-O O-O 10.Bb2 Qe7 11.Rad1 Rad8 2.Rfe1 dxc4 13.bxc4 e5 14.dxe5 Nxe5 15.Nxe5 Bxe5\r16.Qe2 Rxd1 17.Rxd1 Rd8 18.Rxd8+ Qxd8 1-0"

testGame' = "[Event \"Troll Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2001.01.05\"]\r[Round \"1\"]\r[White \"Edvardsen,R\"]\r[Black \"Carlsen,Magnus\"]\r[Result \"1/2-1/2\"]\r[WhiteElo \"2055\"]\r[BlackElo \"\"]\r[ECO \"D12\"]\r\r1.d4 Nf6 2.Nf3 d5 3.e3 Bf5 4.c4 c6 5.Nc3 e6 6.Bd3 Bxd3 7.Qxd3 Nbd7 8.b3 Bd6\r9.O-O O-O 10.Bb2 Qe7 11.Rad1 Rad8 2.Rfe1 dxc4 13.bxc4 e5 14.dxe5 Nxe5 15.Nxe5 Bxe5\r16.Qe2 Rxd1 17.Rxd1 Rd8 18.Rxd8+ 1-0" --Qxd8

testBoard = case (parseCompute testGame) of (Right b) -> b; (Left e) -> board

faultyMove = Capture (Pos Queen B (5, 7)) (4, 8)

faultyBoard = apply testBoard faultyMove

kingFor c = fromJust . find (every [(== King) . piece, (== c) . colour]) . pieces

run = runPrint gameParser