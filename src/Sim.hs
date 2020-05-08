module Sim (run, newGame, readMove, applyMove, moveParser, appliedMoveParser, evaluatedMoveParser, 
            variant, msg, showBoard, showFigure, showCastles, 
            Parser, SimError, Variant, Result (Continue, Retry, Terminate)) where

import qualified Text.Megaparsec as M
import qualified PGN.Parser as P
import qualified Chess as C
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

parseError :: (e -> a) -> (M.ParseError s e -> a) -> M.ParseErrorBundle s e -> a
parseError f h = derive . L.head . M.bundleErrors 
    where derive (M.FancyError _ errors) = f $ strip $ head $ S.toList errors
          derive (other)                 = h other
          strip  (M.ErrorCustom e)       = e

failWith :: SimError -> Parser a
failWith error = M.fancyFailure $ S.fromList [M.ErrorCustom error]

showBoard :: G.Game -> String
showBoard game = D.showBoard (G.mode game) (G.board game)

showFigure :: G.Game -> C.Figure -> String
showFigure game = D.showFigure (G.mode game)

showCastles :: G.Game -> C.Castles -> String
showCastles game = D.showCastles (G.mode game)

showCoord :: G.Game -> C.Square -> String
showCoord game = D.showCoord (G.mode game)

squareFrom :: C.Figure -> C.Coord -> C.Square
squareFrom (piece, colour) coord = (colour, coord)

deriveChessError :: G.Game -> P.ChessError -> SimError
deriveChessError game (P.MissingMovesError)    = SimError GameError "Unavailable move"
deriveChessError game (P.IllegalMoveError)     = SimError GameError "Illegal move" 
deriveChessError game (P.UnexpectedCheckError) = SimError GameError "Board is not in check"
deriveChessError game (P.CaptureError c f)     = SimError GameError ("Cannot capture with " <> showFigure game f <> " at " <> showCoord game (squareFrom f c))
deriveChessError game (P.AdvanceError c f)     = SimError GameError ("Cannot advance with " <> showFigure game f <> " to " <> showCoord game (squareFrom f c)) 
deriveChessError game (P.PromoteError c f)     = SimError GameError ("Cannot promote to " <> showFigure game f <> " at " <>   showCoord game (squareFrom f c))
deriveChessError game (P.CastleError c)        = SimError GameError ("Cannot castle " <> showCastles game c)

deriveParseError :: G.Game -> P.ParseError -> SimError
deriveParseError game = parseError (deriveChessError game) (const $ SimError InputError "Unknown input")

evaluated :: G.Game -> Result 
evaluated game = maybe (Continue game) (Terminate game) $ G.evaluate $ G.board game

-- FixMe `newtype` the tags
newGame :: String -> String -> G.Game
newGame white black = G.Game { G.tags  = [], 
                               G.board = C.newBoard, 
                               G.mode  = D.GameMode }

moveParser :: G.Game -> Parser C.Move
moveParser = fmap snd . appliedMoveParser

appliedMoveParser :: G.Game -> Parser (G.Game, C.Move)
appliedMoveParser game = M.getParserState >>= (parse $ G.board game)
    where parse board state = case (M.runParser' (P.appliedMoveParser board) state) of
                (state, (Right (board', move))) -> M.setParserState state $> (game { G.board = board' }, move)
                (state, (Left err))             -> failWith $ deriveParseError game err

evaluatedMoveParser :: G.Game -> Parser Result
evaluatedMoveParser game = fmap (evaluated . fst) $ appliedMoveParser game

readMove :: String -> G.Game -> Either SimError C.Move
readMove input game = run (moveParser game) input

applyMove :: C.Move -> G.Game -> Result
applyMove move game = maybe (Retry game) (evaluated . add) $ C.applyMove move $ G.board game
    where add board = game { G.board = board }

readApplyMove :: String -> G.Game -> Either SimError Result
readApplyMove move game = run (evaluatedMoveParser game) move

run :: Parser a -> String -> Either SimError a
run parser = either (Left . makeError) Right . M.runParser parser ""
    where makeError = parseError id (const $ SimError InputError "Shit's gone wrong")