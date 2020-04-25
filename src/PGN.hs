module PGN (fromFile, fromFile', parse, parseMany) where

import qualified Text.Megaparsec as M
import qualified Chess.Meta as Chess
import qualified PGN.Internal as PGN

type StringError = String

stringifyError :: Either PGN.ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return

fromFile' :: String -> IO [String]
fromFile' = PGN.fromPGNFile' 

fromFile :: String -> IO (Either StringError [Chess.Game])
fromFile = fmap stringifyError . PGN.fromPGNFile

parse :: String -> Either StringError Chess.Game
parse = stringifyError . PGN.parseGame

parseMany :: String -> Either StringError [Chess.Game]
parseMany = sequence . fmap parse . PGN.fromString'