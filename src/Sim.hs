module Sim (applyMove, readMove, newGame) where

import qualified Chess.Internal as Chess
import qualified Chess.Game as Game
import qualified PGN as P
import qualified Chess as C

type Move = Chess.Move
type Game = Game.Game

newGame :: String -> String -> Game
newGame white black = Game.Game { Game.tags = [], Game.board = Chess.emptyBoard }

applyMove :: Move -> Game -> Maybe Game
applyMove move game = fmap replace $ C.applyMove move $ Game.board game
    where replace board = game { Game.board = board }

readMove :: String -> Game -> Maybe Move
readMove move = either (const Nothing) Just . P.parseMove move . Game.board