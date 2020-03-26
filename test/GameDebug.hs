module GameDebug where

import Lib.Coll
import Data.List
import Data.Maybe
import Chess.Internal2
import PGN.Internal2    

testGame  = "[Event \"Troll Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2001.01.05\"]\r[Round \"1\"]\r[White \"Edvardsen,R\"]\r[Black \"Carlsen,Magnus\"]\r[Result \"1/2-1/2\"]\r[WhiteElo \"2055\"]\r[BlackElo \"\"]\r[ECO \"D12\"]\r\r1.d4 Nf6 2.Nf3 d5 3.e3 Bf5 4.c4 c6 5.Nc3 e6 6.Bd3 Bxd3 7.Qxd3 Nbd7 8.b3 Bd6\r9.O-O O-O 10.Bb2 Qe7 11.Rad1 Rad8 1-0"

testGame' = "[Event \"Troll Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2001.01.05\"]\r[Round \"1\"]\r[White \"Edvardsen,R\"]\r[Black \"Carlsen,Magnus\"]\r[Result \"1/2-1/2\"]\r[WhiteElo \"2055\"]\r[BlackElo \"\"]\r[ECO \"D12\"]\r\r1.d4 Nf6 2.Nf3 d5 3.e3 Bf5 4.c4 c6 5.Nc3 e6 6.Bd3 Bxd3 7.Qxd3 Nbd7 8.b3 Bd6\r 1-0" --9.O-O O-O 10.Bb2 Qe7 1-0"--11.Rad1 Rad8

testBoard = case (parseCompute testGame) of (Right b) -> b; (Left e) -> board

faultyMove = Advance (Pos Bishop W (6, 1)) (4, 3)

faultyBoard = apply testBoard faultyMove

kingFor c = fromJust . find (every [(== King) . piece, (== c) . colour]) . pieces

run = runPrint gameParser