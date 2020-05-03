module Sim (applyMove, readMove, newGame, moveParser, Outcome, Variant, Game, Move) where

import qualified Text.Megaparsec as M
import qualified PGN.Parser as P
import qualified Chess.Display as D
import qualified Chess.Internal as CI
import qualified PGN as PGN
import qualified Chess as Chess
import qualified Chess.Game as Game
import qualified Data.Set as S

data Variant  = InputError | GameError deriving (Ord, Eq, Show)
data SimError = SimError Variant String deriving (Eq, Ord, Show)
data Outcome  = Error Variant String | Terminated Game

data Result = Continue | Terminate
type Parser a = M.Parsec SimError String a

type Move = CI.Move
type Game = Game.Game

showBoard :: Game -> String
showBoard game = D.showBoard (Game.mode game) (Game.board game)

showFigure :: Game -> CI.Figure -> String
showFigure game = D.showFigure (Game.mode game)

showCastles :: Game -> CI.Castles -> String
showCastles game = D.showCastles (Game.mode game)

deriveChessError :: Game -> P.ChessError -> Outcome
deriveChessError game (P.MissingMovesError)    = Error GameError "Unavailable move"
deriveChessError game (P.IllegalMoveError)     = Error GameError "Illegal move" 
deriveChessError game (P.UnexpectedCheckError) = Error GameError "Board is not in check"
deriveChessError game (P.CaptureError c f)     = Error GameError ("Cannot capture with " <> showFigure game f <> " at " <> show c) 
deriveChessError game (P.AdvanceError c f)     = Error GameError ("Cannot advance with " <> showFigure game f <> " to " <> show c) 
deriveChessError game (P.PromoteError c f)     = Error GameError ("Cannot promote to " <> showFigure game f <> " at " <> show c)
deriveChessError game (P.CastleError c)        = Error GameError ("Cannot castle " <> showCastles game c)

deriveParseError :: Game -> P.ParseError -> Outcome
deriveParseError game = maybe (Error InputError "Unknown input") (deriveChessError game) . P.chessError

-- FixMe `newtype` the tags
newGame :: String -> String -> Game
newGame white black = Game.Game { Game.tags  = [], 
                                  Game.board = CI.emptyBoard, 
                                  Game.mode  = D.GameMode }

applyMove :: Move -> Game -> Either Outcome Game
applyMove move game = case (Chess.applyMove move $ Game.board game) of
    (Just board) -> Right game { Game.board = board }
    (Nothing)    -> Left (Error GameError "Illegal move")

readMove :: String -> Game -> Either Outcome Move
readMove smove game = case (PGN.parseMove smove $ Game.board game) of
    (Left err)    -> Left (deriveParseError game err)
    (Right move)  -> Right move

-- `Play` uses parers. If I am to use this for the game simulation in `Play`, I have to somehow make this return a parser, whose error is probably an `Outcome`
readApplyMove :: String -> Game -> Either Outcome Game
readApplyMove smove game = either Left (eval . apply) $ readMove smove game
    where apply move            = game { Game.board = CI.forceApply (Game.board game) move }
          eval game             = maybe (Right game) (terminate game) $ Game.evaluate (Game.board game) -- Add tags
          terminate game reason = Left (Terminated game { Game.tags = (Game.Termination reason) : (Game.tags game) })


failWith :: SimError -> Parser a
failWith error = M.fancyFailure $ S.fromList [M.ErrorCustom error]

--- This is the only possibility that I've found for transforming one parser into another
-- I get the state of my current one, apply the other one on it, return the state after application
-- set the state of my old one to the new one I've gotten after application
moveParser :: Game -> Parser Move
moveParser game = M.getParserState >>= (parse $ Game.board game)
    where parse board state = case (M.runParser' (P.moveParser board) state) of
            (state, (Right move)) -> fmap (const move) $ M.setParserState state
            (state, (Left err))   -> failWith (SimError GameError "I like big butts and I cannot lie")
                                                                