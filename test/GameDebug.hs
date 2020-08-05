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
import qualified Chess as C
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (fromRight)

pgnGames = unsafePerformIO $ C.pgnFromFile "./test/resources/games/carlsen.pgn"

game = C.parseGame $ head pgnGames

move = fmap (head . I.past . C.gameBoard) game

runPrint :: String -> IO ()
runPrint =  putStrLn . either M.errorBundlePretty show . run (L.moveParser I.emptyBoard)