module GameDebug where

import Lib.Coll
import Lib.Megaparsec
import Data.List
import Data.Maybe
import qualified Chess.Internal as I
import qualified Text.Megaparsec as M
import qualified Chess.Game as G
import qualified Writer.LAN as W
import qualified Parser.LAN as L
import qualified Writer.PGN as P
import qualified Chess as C
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (fromRight)

pgnGames = unsafePerformIO $ C.pgnFromFile "./test/resources/games/carlsen.pgn"

game = C.parseGame $ head pgnGames

right x = case x of (Right a) -> a

pgnVersion = right $ fmap (P.writeFor . G.gameBoard) game
lanVersion = right $ fmap (W.writeFor . G.gameBoard) game

complete = unlines $ fmap (\(a, b) -> a <> " :: " <> b) $ zip pgnVersion lanVersion