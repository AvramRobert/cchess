module GameDebug where

import Lib.Coll
import Data.List
import Data.Maybe
import Chess.Internal2
import PGN.Internal2    

testGame = "[Event \"Troll Masters\"]\r[Site \"Gausdal NOR\"]\r[Date \"2001.01.05\"]\r[Round \"1\"]\r[White \"Edvardsen,R\"]\r[Black \"Carlsen,Magnus\"]\r[Result \"1/2-1/2\"]\r[WhiteElo \"2055\"]\r[BlackElo \"\"]\r[ECO \"D12\"]\r\r1.d4 Nf6 2.Nf3 d5 1-0"

testBoard = case (parseCompute testGame) of (Right b) -> b; (Left e) -> board

faultyMove = Advance (Pos Pawn W (5, 2)) (5, 3)

faultyBoard = apply testBoard faultyMove

faultyPieces = filter ((== B) . colour) $ pieces faultyBoard

checkMoves :: Coord -> [Move]
checkMoves = movesPosition faultyBoard . Pos Pawn B

kingFor c = fromJust . find (every [(== King) . piece, (== c) . colour]) . pieces