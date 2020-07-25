module Chess (
    ParserTie, ChessResult (Terminate, Continue, Retry), Error (Error), Variant (InputError, GameError, ParseError),
    C.Move (C.Castle, C.Promote, C.Advance, C.Capture, C.Enpassant), C.Castles,
    C.Position (C.Pos), C.Figure, C.Square, C.Colour (C.W, C.B), C.Coord, G.Game,
    G.Event, G.Site, G.Date, G.Round, G.White, G.Black, G.Result, G.WhiteElo, G.BlackElo,
    G.WhiteTitle, G.BlackTitle, G.WhiteUSCF, G.BlackUSCF, G.WhiteNA, G.BlackNA, G.WhiteType,
    G.BlackType, G.EventDate, G.EventSponsor, G.Section, G.Stage, G.Board, G.Opening, G.Variation,
    G.SubVariation, G.ECO, G.NIC, G.Time, G.UTCTime, G.UTCDate, G.TimeControl, G.SetUp, G.FEN, 
    G.Termination, G.PlyCount, G.Annotator, G.Mode, G.Unknown, G.Address (Address, NoAddress),
    G.Rating (G.Rated, G.Unrated), G.Outcome (G.WhiteWin, G.BlackWin, G.Draw, G.Other),
    G.Title (G.GM, G.FM, G.IM, G.UT), G.PlayerType (G.Human, G.Computer), G.GameMode (G.OTB, G.ICS),
    G.Reason (G.Abandoned, G.Adjundication, G.Death, G.Emergency, G.Normal, G.Checkmate, 
              G.Resignation, G.Stalemate, G.Infraction, G.TimeForfeit, G.Unterminated),
    newGame, quickGame, legalMoves, currentPlayer, appliedMoveParser, evaluatedMoveParser, moveParser, pgnFromFile,
    gamesFromFile, evaluate, writeMove, writeGame, parseGame, parseManyGames, terminationReason, variant, message,
    applyMove, parseMove, parseApplyMove, writeFen, getInput, setInput, failWith, movesFor, currentPlayerMoves, G.entries, G.gameBoard,
    G.locate, G.tag, G.event, G.site, G.date, G.round, G.white, G.black, G.result, G.whiteElo, G.blackElo,
    G.whiteTitle, G.blackTitle, G.whiteUSCF, G.blackUSCF, G.whiteNA, G.blackNA, G.whiteType, G.blackType,
    G.subVariation, G.eco, G.nic, G.time, G.utcTime, G.utcDate, G.timeControl, G.setup, G.fen, G.termination,
    G.plyCount, G.annotator, G.mode, G.unknown, G.rating, G.address, G.overTheBoard, G.internetServer) where 

import qualified Text.Megaparsec as M        
import qualified Chess.Internal as C
import qualified Chess.Display as D
import qualified Chess.Game as G
import qualified PGN.Parser as P
import qualified PGN.Writer as W
import Chess.Game (Game (Game), entries, gameBoard, createGame, Reason)
import Lib.Megaparsec (customError, run)
import Data.Functor (($>))

type ParserState a = M.State a P.ChessError

data Variant = InputError 
             | GameError 
             | ParseError
             deriving (Show, Eq, Ord)

data Error = Error { variant :: Variant, 
                     message :: String }
            deriving (Show, Eq, Ord)

data ChessResult = Terminate Game Reason
                 | Continue  Game
                 | Retry     Game
                 deriving (Show)

class ParserTie p where
    getInput  :: p String
    setInput  :: String -> p ()
    failWith  :: Error -> p a

deriveParseError :: P.ParseError -> Error
deriveParseError = customError asError (const $ Error InputError "Unknown input")
    where asError (P.MissingMovesError)    = Error GameError "Unavailable move"
          asError (P.IllegalMoveError)     = Error GameError "Illegal move" 
          asError (P.UnexpectedCheckError) = Error GameError "Board is not in check"
          asError (P.CaptureError c f)     = Error GameError ("Cannot capture "    <> showFigure f <> " at " <> showCoord (squareFrom f c))
          asError (P.AdvanceError c f)     = Error GameError ("Cannot advance "    <> showFigure f <> " to " <> showCoord (squareFrom f c)) 
          asError (P.PromoteError c f)     = Error GameError ("Cannot promote to " <> showFigure f <> " at " <> showCoord (squareFrom f c))
          asError (P.CastleError c)        = Error GameError ("Cannot castle "     <> showCastles c)
          showFigure                       = D.showFigure  D.ErrorMode
          showCoord                        = D.showCoord   D.ErrorMode
          showCastles                      = D.showCastles D.ErrorMode
          squareFrom (piece, colour) coord = (colour, coord)
    
fromParseError :: Either P.ParseError a -> Either Error a
fromParseError (Left err) = Left (deriveParseError err)
fromParseError (Right a)  = Right a

runParser :: P.Parser a -> (a -> b) -> String -> Either Error b
runParser parser f = either (Left . deriveParseError) (Right . f) . run parser 

runOn :: String -> P.Parser a -> P.Parser (ParserState String, Either P.ParseError a)
runOn input parser = do
        _     <- M.setInput input
        state <- M.getParserState
        return (M.runParser' parser state)

runInternalParser :: P.Parser a -> String -> (ParserState String, Either Error a)
runInternalParser parser input = either (\_ -> error "this will never fail") id $ run conversion ""
    where convert (state, Right a)  = (state, Right a)
          convert (state, Left err) = (state, Left (deriveParseError err))
          conversion                = fmap convert $ runOn input parser          

bootstrap :: (ParserTie p, Monad p) => P.Parser a -> p a
bootstrap parser = getInput >>= (reattach . runInternalParser parser)
    where reattach (state, Right r)  = setInput (M.stateInput state) $> r
          reattach (state, Left err) = setInput (M.stateInput state) >> failWith err 

appliedMoveParser :: (ParserTie p, Monad p) => Game -> p (Game, C.Move)
appliedMoveParser game = fmap add $ bootstrap $ P.appliedMoveParser $ gameBoard game
    where add (board, move) = (game { gameBoard = board }, move)

moveParser :: (ParserTie p, Monad p) => Game -> p C.Move
moveParser = fmap snd . appliedMoveParser

evaluatedMoveParser :: (ParserTie p, Monad p) => Game -> p ChessResult
evaluatedMoveParser game = fmap (evaluate . fst) $ appliedMoveParser game

-- this could, at complile time, check if the string ends with a `.pgn`
pgnFromFile :: String -> IO [String]
pgnFromFile = P.fromPGNFile' 

gamesFromFile :: String -> IO (Either Error [Game])
gamesFromFile = fmap fromParseError . P.fromPGNFile

parseGame :: String -> Either Error Game
parseGame = fromParseError . P.parseGame

parseManyGames :: String -> Either Error [Game]
parseManyGames = sequence . fmap parseGame . P.fromString'

writeGame :: Game -> String
writeGame = unlines . W.writeMoves . gameBoard

writeMove :: C.Move -> Game -> Maybe String
writeMove move = W.writeMove move . gameBoard

writeFen :: Game -> String
writeFen = W.fen . gameBoard

applyMove :: C.Move -> Game -> ChessResult
applyMove move game = maybe (Retry game) (evaluate . add) $ C.apply (gameBoard game) move 
    where add board = game { gameBoard = board }

parseMove :: String -> Game -> Either Error C.Move
parseMove move game = runParser (P.moveParser (gameBoard game)) id move

parseApplyMove :: String -> Game -> Either Error ChessResult
parseApplyMove move game = runParser (P.appliedMoveParser (gameBoard game)) (evaluate . add) move
    where add (board, _) = game { gameBoard = board }

legalMoves :: Game -> [C.Move]
legalMoves =  C.allMoves . gameBoard

currentPlayer :: Game -> C.Colour
currentPlayer = C.player . gameBoard

movesFor :: C.Colour -> Game -> [C.Move]
movesFor colour game = C.movesFor (gameBoard game) colour

currentPlayerMoves :: Game -> [C.Move]
currentPlayerMoves game = movesFor (currentPlayer game) game

terminationReason :: Game -> Maybe Reason
terminationReason game = let board   = gameBoard game
                             immoble = C.immoble board
                             checked = C.check board
                         in if (checked && immoble) then Just G.Checkmate
                         else if immoble            then Just G.Stalemate
                         else                            Nothing

evaluate :: Game -> ChessResult
evaluate game = maybe (Continue game) (Terminate game) $ terminationReason game

newGame   = createGame
quickGame = newGame (G.event "CCHESS Quick Game")
                    (G.site  "CCHESS Platform")
                    (G.date  "Today")
                    (G.round "-")
                    (G.white "CCHESS Player 1")
                    (G.black "CCHESS Player 2")