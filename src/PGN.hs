module PGN (fromFile, parse, compute, fromFileString) where

import qualified Text.Megaparsec as M
import qualified Chess.Internal as Chess
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
compute = fmap head . computeMany

computeMany :: String -> Either StringError [Chess.Board]
computeMany games = stringifyError (PGN.fromString games) >>= (sequence . fmap runGame)

runGame :: PGN.Game -> Either StringError Chess.Board
runGame = either (Left . show) (Right) . run . PGN.moves
    where run = foldl (\b m -> b >>= (Chess.move m)) (Right Chess.board)