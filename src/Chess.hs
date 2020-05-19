module Chess (
    newGame, legalMoves, currentPlayer, appliedMoveParser, evaluatedMoveParser, moveParser, pgnFromFile,
    gameFromFile, evaluate, writeMove, writeGame, parseGame, parseManyGames, termination, variant, message,
    getInput, setInput, failWith,
    ParserTie, Result (Terminate, Continue, Retry), Error (Error), Variant (InputError, GameError, ParseError),
    C.Move (C.Castle, C.Promote, C.Advance, C.Capture, C.Enpassant), C.Castles,
    C.Board, C.Position (C.Pos), C.Figure, C.Square, C.Colour (C.W, C.B), C.Coord) where 

import qualified Text.Megaparsec as M        
import qualified Chess.Game as G
import qualified Chess.Internal as C
import qualified Chess.Display as D
import qualified PGN.Parser as P
import qualified PGN.Writer as W
import Lib.Megaparsec (customError)
import Data.Functor (($>))
import Lib.Megaparsec

data Variant = InputError 
             | GameError 
             | ParseError
             deriving (Show, Eq, Ord)

data Error = Error { variant :: Variant, 
                     message :: String }
            deriving (Show, Eq, Ord)

data Result = Terminate G.Game G.Reason
            | Continue  G.Game
            | Retry     G.Game

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

runOn :: String -> P.Parser a -> P.Parser (M.State String, Either P.ParseError a)
runOn input parser = do
        _     <- M.setInput input
        state <- M.getParserState
        return (M.runParser' parser state)

runInternalParser :: P.Parser a -> String -> (M.State String, Either Error a)
runInternalParser parser input = either (\_ -> error "this will never fail") id $ run conversion ""
    where convert (state, Right a)  = (state, Right a)
          convert (state, Left err) = (state, Left (deriveParseError err))
          conversion                = fmap convert $ runOn input parser          

bootstrap :: (ParserTie p, Monad p) => P.Parser a -> p a
bootstrap parser = getInput >>= (reattach . runInternalParser parser)
    where reattach (state, Right r)  = setInput (M.stateInput state) $> r
          reattach (state, Left err) = setInput (M.stateInput state) >> failWith err 

appliedMoveParser :: (ParserTie p, Monad p) => G.Game -> p (G.Game, C.Move)
appliedMoveParser game = fmap add $ bootstrap $ P.appliedMoveParser $ G.board game
    where add (board, move) = (game { G.board = board }, move)

moveParser :: (ParserTie p, Monad p) => G.Game -> p C.Move
moveParser = fmap snd . appliedMoveParser

evaluatedMoveParser :: (ParserTie p, Monad p) => G.Game -> p Result
evaluatedMoveParser game = fmap (evaluate . fst) $ appliedMoveParser game

pgnFromFile :: String -> IO [String]
pgnFromFile = P.fromPGNFile' 

gameFromFile :: String -> IO (Either Error [G.Game])
gameFromFile = fmap fromParseError . P.fromPGNFile

parseGame :: String -> Either Error G.Game
parseGame = fromParseError . P.parseGame

parseManyGames :: String -> Either Error [G.Game]
parseManyGames = sequence . fmap parseGame . P.fromString'

writeGame :: G.Game -> String
writeGame = unlines . W.writeMoves . G.board

writeMove :: C.Move -> G.Game -> Maybe String
writeMove move = W.writeMove move . G.board

applyMove :: C.Move -> G.Game -> Result
applyMove move game = maybe (Retry game) (evaluate . add) $ C.apply (G.board game) move 
    where add board = game { G.board = board }

parseMove :: String -> G.Game -> Either Error C.Move
parseMove move game = runParser (P.moveParser (G.board game)) id move

parseApplyMove :: String -> G.Game -> Either Error Result
parseApplyMove move game = runParser (P.appliedMoveParser (G.board game)) (evaluate . add) move
    where add (board, _) = game { G.board = board }

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