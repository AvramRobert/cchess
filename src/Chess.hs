module Chess where 

import qualified Chess.Internal as Chess
import Data.Set (Set)

newBoard :: Chess.Board
newBoard = Chess.board

applyMove :: Chess.Move -> Chess.Board -> Either Chess.Outcome Chess.Board
applyMove m b = Chess.move m b

legalMoves :: Chess.Board -> Set Chess.Move
legalMoves = Chess.legalMoves'

currentPlayer :: Chess.Board -> Chess.Colour
currentPlayer = Chess.player