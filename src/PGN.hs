module PGN (fromFile, fromFile', parse, parseMany) where

import qualified Text.Megaparsec as M
import Chess.Game (Game)
import PGN.Parser (ParseError, fromPGNFile, fromPGNFile', fromString', parseGame)

type StringError = String

stringifyError :: Either ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return

fromFile' :: String -> IO [String]
fromFile' = fromPGNFile' 

fromFile :: String -> IO (Either StringError [Game])
fromFile = fmap stringifyError . fromPGNFile

parse :: String -> Either StringError Game
parse = stringifyError . parseGame

parseMany :: String -> Either StringError [Game]
parseMany = sequence . fmap parse . fromString'