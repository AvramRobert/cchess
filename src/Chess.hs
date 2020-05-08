module Chess (
    newBoard, applyMove, legalMoves, currentPlayer,
    Chess.Move (Chess.Castle, Chess.Promote, Chess.Advance, Chess.Capture, Chess.Enpassant), Chess.Castles,
    Chess.Board, Chess.Position (Chess.Pos), Chess.Figure, Chess.Square, Chess.Colour (Chess.W, Chess.B), Chess.Coord) where 
        
import qualified Chess.Game as Game
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