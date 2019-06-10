module PGN (fromFile, parse, compute, fromFileString) where

import qualified Text.Megaparsec as M
import qualified Chess as Chess
import qualified PGN.Internal as PGN

type StringError = String

stringifyError :: Either PGN.ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return 

fromFileString :: String -> IO [String]
fromFileString = PGN.fromPGNFileString 

fromFile :: String -> IO (Either StringError [PGN.Game])
fromFile = fmap stringifyError . PGN.fromPGNFile

parse :: String -> Either StringError [Chess.Move]
parse = stringifyError . PGN.parseGame

compute :: String -> Either StringError Chess.Board
compute game = (parse game) >>= (either (Left . show) (Right) . computeGame)
    where computeGame = foldl (\b m -> b >>= (Chess.move m)) (Right Chess.board)