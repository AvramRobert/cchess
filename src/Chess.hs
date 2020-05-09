module Chess (
    newGame, applyMove, parseApplyMove, legalMoves, currentPlayer, appliedMoveParser, evaluatedMoveParser, moveParser, pgnFromFile,
    gameFromFile, evaluated, writeMove, writeGame, parseGame, parseManyGames, showBoard, showFigure, showCastles, showCoord,
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

squareFrom :: C.Figure -> C.Coord -> C.Square
squareFrom (piece, colour) coord = (colour, coord)

deriveChessError :: G.Game -> P.ChessError -> Error
deriveChessError game (P.MissingMovesError)    = Error GameError "Unavailable move"
deriveChessError game (P.IllegalMoveError)     = Error GameError "Illegal move" 
deriveChessError game (P.UnexpectedCheckError) = Error GameError "Board is not in check"
deriveChessError game (P.CaptureError c f)     = Error GameError ("Cannot capture with " <> showFigure game f <> " at " <> showCoord game (squareFrom f c))
deriveChessError game (P.AdvanceError c f)     = Error GameError ("Cannot advance with " <> showFigure game f <> " to " <> showCoord game (squareFrom f c)) 
deriveChessError game (P.PromoteError c f)     = Error GameError ("Cannot promote to " <> showFigure game f <> " at " <>   showCoord game (squareFrom f c))
deriveChessError game (P.CastleError c)        = Error GameError ("Cannot castle " <> showCastles game c)

deriveParseError :: G.Game -> P.ParseError -> Error
deriveParseError game = customError (deriveChessError game) (const $ Error InputError "Unknown input")

showBoard :: G.Game -> String
showBoard game = D.showBoard (G.mode game) (G.board game)

showFigure :: G.Game -> C.Figure -> String
showFigure game = D.showFigure (G.mode game)

showCastles :: G.Game -> C.Castles -> String
showCastles game = D.showCastles (G.mode game)

showCoord :: G.Game -> C.Square -> String
showCoord game = D.showCoord (G.mode game)

evaluated :: G.Game -> Result 
evaluated game = maybe (Continue game) (Terminate game) $ G.evaluate $ G.board game

moveParser :: G.Game -> Parser C.Move
moveParser = fmap snd . appliedMoveParser

appliedMoveParser :: G.Game -> Parser (G.Game, C.Move)
appliedMoveParser game = M.getParserState >>= (parse $ G.board game)
    where parse board state = case (M.runParser' (P.appliedMoveParser board) state) of
                (state, (Right (board', move))) -> M.setParserState state $> (game { G.board = board' }, move)
                (state, (Left err))             -> failWith $ deriveParseError game err

evaluatedMoveParser :: G.Game -> Parser Result
evaluatedMoveParser game = fmap (evaluated . fst) $ appliedMoveParser game

pgnFromFile :: String -> IO [String]
pgnFromFile = P.fromPGNFile' 

-- Make these use `Error` instead of this dumb `StringError`
type StringError = String

stringifyError :: Either P.ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return

gameFromFile :: String -> IO (Either StringError [G.Game])
gameFromFile = fmap stringifyError . P.fromPGNFile

parseGame :: String -> Either StringError G.Game
parseGame = stringifyError . P.parseGame

parseManyGames :: String -> Either StringError [G.Game]
parseManyGames = sequence . fmap parseGame . P.fromString'
--

parseMove :: String -> G.Game -> Either Error C.Move
parseMove input game = runParser (moveParser game) input 

writeGame :: G.Game -> String
writeGame = unlines . W.writeMoves . G.board

writeMove :: C.Move -> G.Game -> Maybe String
writeMove move = W.writeMove move . G.board

applyMove :: C.Move -> G.Game -> Result
applyMove move game = maybe (Retry game) (evaluated . add) $ C.apply (G.board game) move 
    where add board = game { G.board = board }

parseApplyMove :: String -> G.Game -> Either Error Result
parseApplyMove move game = runParser (evaluatedMoveParser game) move

legalMoves :: G.Game -> [C.Move]
legalMoves =  C.allMoves . G.board

currentPlayer :: G.Game -> C.Colour
currentPlayer = C.player . G.board

-- FixMe `newtype` the tags
newGame :: String -> String -> G.Game
newGame white black = G.Game { G.tags  = [], 
                               G.board = C.emptyBoard, 
                               G.mode  = D.GameMode }