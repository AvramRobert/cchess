module Sim (run, newGame, readMove, applyMove, moveParser, appliedMoveParser, 
            variant, msg, showBoard, showFigure, showCastles, 
            Parser, SimError, Variant, Game, Board, Move, Result (Continue, Retry, Terminate)) where

import qualified Text.Megaparsec as M
import qualified Chess.Internal as C
import qualified PGN.Parser as P
import qualified Chess.Display as D
import qualified Chess.Game as G
import qualified Data.List.NonEmpty as L
import qualified Data.Set as S
import Data.Functor (($>))

type Parser a = M.Parsec SimError String a

data Variant  = InputError | GameError deriving (Ord, Eq, Show)
data SimError = SimError { variant :: Variant, msg :: String } deriving (Eq, Ord, Show)

data Result = Continue  G.Game 
            | Retry     G.Game 
            | Terminate G.Game G.Reason

type Move  = C.Move
type Board = C.Board
type Game  = G.Game

parseError :: (e -> a) -> (M.ParseError s e -> a) -> M.ParseErrorBundle s e -> a
parseError f h = derive . L.head . M.bundleErrors 
    where derive (M.FancyError _ errors) = f $ strip $ head $ S.toList errors
          derive (other)                 = h other
          strip  (M.ErrorCustom e)       = e

failWith :: SimError -> Parser a
failWith error = M.fancyFailure $ S.fromList [M.ErrorCustom error]

showBoard :: Game -> String
showBoard game = D.showBoard (G.mode game) (G.board game)

showFigure :: Game -> C.Figure -> String
showFigure game = D.showFigure (G.mode game)

showCastles :: Game -> C.Castles -> String
showCastles game = D.showCastles (G.mode game)

showCoord :: Game -> C.Square -> String
showCoord game = D.showCoord (G.mode game)

squareFrom :: C.Figure -> C.Coord -> C.Square
squareFrom (piece, colour) coord = (colour, coord)

deriveChessError :: Game -> P.ChessError -> SimError
deriveChessError game (P.MissingMovesError)    = SimError GameError "Unavailable move"
deriveChessError game (P.IllegalMoveError)     = SimError GameError "Illegal move" 
deriveChessError game (P.UnexpectedCheckError) = SimError GameError "Board is not in check"
deriveChessError game (P.CaptureError c f)     = SimError GameError ("Cannot capture with " <> showFigure game f <> " at " <> showCoord game (squareFrom f c))
deriveChessError game (P.AdvanceError c f)     = SimError GameError ("Cannot advance with " <> showFigure game f <> " to " <> showCoord game (squareFrom f c)) 
deriveChessError game (P.PromoteError c f)     = SimError GameError ("Cannot promote to " <> showFigure game f <> " at " <>   showCoord game (squareFrom f c))
deriveChessError game (P.CastleError c)        = SimError GameError ("Cannot castle " <> showCastles game c)

deriveParseError :: Game -> P.ParseError -> SimError
deriveParseError game = parseError (deriveChessError game) (const $ SimError InputError "Unknown input")

evaluated :: Game -> Maybe Board -> Result 
evaluated game (Nothing)    = Retry game
evaluated game (Just board) = maybe (continueWith board) (terminateWith board) $ G.evaluate board
    where continueWith board  = Continue game { G.board = board }
          terminateWith board = Terminate game { G.board = board } 

-- FixMe `newtype` the tags
newGame :: String -> String -> Game
newGame white black = G.Game { G.tags  = [], 
                               G.board = C.emptyBoard, 
                               G.mode  = D.GameMode }

moveParser :: Game -> Parser Move
moveParser game = M.getParserState >>= (parse $ G.board game)
    where parse board state = case (M.runParser' (P.moveParser board) state) of
            (state, (Right move)) -> M.setParserState state $> move
            (state, (Left err))   -> failWith $ deriveParseError game err

appliedMoveParser :: Game -> Parser Result
appliedMoveParser game = fmap (evaluated game . apply) $ moveParser game
    where apply = return . C.forceApply (G.board game)

readMove :: String -> Game -> Either SimError Move
readMove input game = run (moveParser game) input

applyMove :: Move -> Game -> Result
applyMove move game = evaluated game $ C.apply (G.board game) move

readApplyMove :: String -> Game -> Either SimError Result
readApplyMove move game = run (appliedMoveParser game) move

run :: Parser a -> String -> Either SimError a
run parser = either (Left . makeError) Right . M.runParser parser ""
    where makeError = parseError id (const $ SimError InputError "Shit's gone wrong")