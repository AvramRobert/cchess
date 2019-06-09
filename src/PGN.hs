module PGN (fromFile, parse, compute) where

import qualified Text.Megaparsec as M
import qualified Chess as Chess
import qualified PGN.Internal as PGN

-- TODO: 
-- 1. Write a proper record to keep additional details of PGN games (year, location, players etc)

type StringError = String

fromFile :: String -> IO [PGN.Game]
fromFile = PGN.fromPGNFile

parse :: String -> Either StringError [Chess.Move]
parse = either (Left . M.errorBundlePretty) (Right) . PGN.parseGame

compute :: String -> Either StringError Chess.Board
compute game = (either (Left . M.errorBundlePretty) (Right) $ PGN.parseGame game) >>= (either (Left . show) (Right) . computeGame)
    where computeGame = foldl (\b m -> b >>= (Chess.move m)) (Right Chess.board)