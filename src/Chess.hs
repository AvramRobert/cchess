module Chess where 

import qualified Chess.Internal as Chess
import Data.Set (Set)

newBoard :: Chess.Board
newBoard = Chess.emptyBoard

applyMove :: Chess.Move -> Chess.Board -> Maybe Chess.Board
applyMove move board = Chess.apply board move

legalMoves :: Chess.Board -> [Chess.Move]
legalMoves = Chess.allMoves

currentPlayer :: Chess.Board -> Chess.Colour
currentPlayer = Chess.player