module GameDebug where

import Lib.Coll
import Lib.Megaparsec
import Data.List
import Data.Maybe
import qualified Chess.Internal as C
import qualified Text.Megaparsec as M
import qualified Chess.Game as G
import qualified Writer.PGN as W
import qualified Parser.LAN as L
import qualified Chess as C
import System.IO.Unsafe (unsafePerformIO)

runPrint :: String -> IO ()
runPrint =  putStrLn . either M.errorBundlePretty show . run (L.moveParser C.emptyBoard)