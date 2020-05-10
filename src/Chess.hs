module Chess (
    newGame, applyMove, parseApplyMove, legalMoves, currentPlayer, appliedMoveParser, evaluatedMoveParser, moveParser, pgnFromFile,
    gameFromFile, evaluate, writeMove, writeGame, parseGame, parseManyGames, termination,
    Result (Terminate, Continue, Retry),    
    C.Move (C.Castle, C.Promote, C.Advance, C.Capture, C.Enpassant), C.Castles,
    C.Board, C.Position (C.Pos), C.Figure, C.Square, C.Colour (C.W, C.B), C.Coord) where 

import qualified Text.Megaparsec as M        
import qualified Chess.Game as G
import qualified Chess.Internal as C
import qualified Chess.Display as D
import qualified PGN.Parser as P
import qualified PGN.Writer as W
import Data.Functor (($>))
import API

data Result = Terminate G.Game G.Reason
            | Continue  G.Game
            | Retry     G.Game

deriveParseError :: P.ParseError -> Error
deriveParseError = customError asError (const $ Error InputError "Unknown input")
    where asError (P.MissingMovesError)    = Error GameError "Unavailable move"
          asError (P.IllegalMoveError)     = Error GameError "Illegal move" 
          asError (P.UnexpectedCheckError) = Error GameError "Board is not in check"
          asError (P.CaptureError c f)     = Error GameError ("Cannot capture " <> showFigure f <> " at " <> showCoord (squareFrom f c))
          asError (P.AdvanceError c f)     = Error GameError ("Cannot advance " <> showFigure f <> " to " <> showCoord (squareFrom f c)) 
          asError (P.PromoteError c f)     = Error GameError ("Cannot promote to "   <> showFigure f <> " at " <> showCoord (squareFrom f c))
          asError (P.CastleError c)        = Error GameError ("Cannot castle "       <> showCastles c)
          showFigure                       = D.showFigure  D.ErrorMode
          showCoord                        = D.showCoord   D.ErrorMode
          showCastles                      = D.showCastles D.ErrorMode
          squareFrom (piece, colour) coord = (colour, coord)
    
fromParseError :: Either P.ParseError a -> Either Error a
fromParseError (Left err) = Left (deriveParseError err)
fromParseError (Right a)  = Right a

moveParser :: G.Game -> Parser C.Move
moveParser = fmap snd . appliedMoveParser

appliedMoveParser :: G.Game -> Parser (G.Game, C.Move)
appliedMoveParser game = M.getParserState >>= (parse $ G.board game)
    where parse board state = case (M.runParser' (P.appliedMoveParser board) state) of
                (state, (Right (board', move))) -> M.setParserState state $> (game { G.board = board' }, move)
                (state, (Left err))             -> failWith $ deriveParseError err

evaluatedMoveParser :: G.Game -> Parser Result
evaluatedMoveParser game = fmap (evaluate . fst) $ appliedMoveParser game

pgnFromFile :: String -> IO [String]
pgnFromFile = P.fromPGNFile' 

gameFromFile :: String -> IO (Either Error [G.Game])
gameFromFile = fmap fromParseError . P.fromPGNFile

parseGame :: String -> Either Error G.Game
parseGame = fromParseError . P.parseGame

parseManyGames :: String -> Either Error [G.Game]
parseManyGames = sequence . fmap parseGame . P.fromString'

parseMove :: String -> G.Game -> Either Error C.Move
parseMove input game = runParser (moveParser game) input 

writeGame :: G.Game -> String
writeGame = unlines . W.writeMoves . G.board

writeMove :: C.Move -> G.Game -> Maybe String
writeMove move = W.writeMove move . G.board

applyMove :: C.Move -> G.Game -> Result
applyMove move game = maybe (Retry game) (evaluate . add) $ C.apply (G.board game) move 
    where add board = game { G.board = board }

parseApplyMove :: String -> G.Game -> Either Error Result
parseApplyMove move game = runParser (evaluatedMoveParser game) move

legalMoves :: G.Game -> [C.Move]
legalMoves =  C.allMoves . G.board

currentPlayer :: G.Game -> C.Colour
currentPlayer = C.player . G.board

termination :: G.Game -> Maybe G.Reason
termination game = let board   = G.board game
                       immoble = C.immoble board
                       checked = C.check board
                   in if (checked && immoble) then Just G.Checkmate
                      else if immoble         then Just G.Stalemate
                      else                         Nothing

evaluate :: G.Game -> Result
evaluate game = maybe (Continue game) (Terminate game) $ termination game

-- FixMe `newtype` the tags
newGame :: String -> String -> G.Game
newGame white black = G.Game { G.tags  = [], 
                               G.board = C.emptyBoard }