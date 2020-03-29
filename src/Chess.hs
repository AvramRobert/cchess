module Chess where 

import qualified Chess.Internal as Chess
import Data.Set (Set)

newBoard :: Chess.Board
newBoard = Chess.board

applyMove :: Chess.Board -> Chess.Move -> Either Chess.Outcome Chess.Board
applyMove = Chess.perform

legalMoves :: Chess.Board -> [Chess.Move]
legalMoves = Chess.allMoves

currentPlayer :: Chess.Board -> Chess.Colour
currentPlayer = Chess.player