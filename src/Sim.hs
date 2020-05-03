module Sim (run, newGame, moveParser, variant, msg, showBoard, showFigure, showCastles, Parser, SimError, Variant, Game, Move) where

import qualified Text.Megaparsec as M
import qualified PGN.Parser as P
import qualified Chess.Display as D
import qualified Chess.Internal as CI
import qualified PGN as PGN
import qualified Chess as Chess
import qualified Chess.Game as Game
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NL
import Data.Functor (($>))

type Parser a = M.Parsec SimError String a

data Variant  = InputError | GameError deriving (Ord, Eq, Show)
data SimError = SimError { variant :: Variant, msg :: String } deriving (Eq, Ord, Show)

data Result = Continue | Terminate

type Move = CI.Move
type Game = Game.Game

failWith :: SimError -> Parser a
failWith error = M.fancyFailure $ S.fromList [M.ErrorCustom error]

showBoard :: Game -> String
showBoard game = D.showBoard (Game.mode game) (Game.board game)

showFigure :: Game -> CI.Figure -> String
showFigure game = D.showFigure (Game.mode game)

showCastles :: Game -> CI.Castles -> String
showCastles game = D.showCastles (Game.mode game)

deriveChessError :: Game -> P.ChessError -> SimError
deriveChessError game (P.MissingMovesError)    = SimError GameError "Unavailable move"
deriveChessError game (P.IllegalMoveError)     = SimError GameError "Illegal move" 
deriveChessError game (P.UnexpectedCheckError) = SimError GameError "Board is not in check"
deriveChessError game (P.CaptureError c f)     = SimError GameError ("Cannot capture with " <> showFigure game f <> " at " <> show c) 
deriveChessError game (P.AdvanceError c f)     = SimError GameError ("Cannot advance with " <> showFigure game f <> " to " <> show c) 
deriveChessError game (P.PromoteError c f)     = SimError GameError ("Cannot promote to " <> showFigure game f <> " at " <> show c)
deriveChessError game (P.CastleError c)        = SimError GameError ("Cannot castle " <> showCastles game c)

deriveParseError :: Game -> P.ParseError -> SimError
deriveParseError game = maybe (SimError InputError "Unknown input") (deriveChessError game) . P.chessError

-- FixMe `newtype` the tags
newGame :: String -> String -> Game
newGame white black = Game.Game { Game.tags  = [], 
                                  Game.board = CI.emptyBoard, 
                                  Game.mode  = D.GameMode }

moveParser :: Game -> Parser Move
moveParser game = M.getParserState >>= (parse $ Game.board game)
    where parse board state = case (M.runParser' (P.moveParser board) state) of
            (state, (Right move)) -> M.setParserState state $> move
            (state, (Left err))   -> failWith $ deriveParseError game err

-- I don't think PGN needs the `chessError` extraction anymore. Given that this happenes here
run :: Parser a -> String -> Either SimError a
run parser = makeSimError . M.runParser parser ""
    where makeSimError (Right a)              = Right a
          makeSimError (Left err)             = deriveError $ NL.head $ M.bundleErrors err
          deriveError (M.FancyError _ errors) = Left (strip $ head $ S.toList errors)
          deriveError (_)                     = Left (SimError InputError "Shit's gone wrong")
          strip (M.ErrorCustom e)             = e

-- add a function that parses a move and applies it with evaluation, returning the game
-- FIXME: Clean this up a little bit. Make it a bit more usable and general.