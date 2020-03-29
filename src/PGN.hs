module PGN (fromFile, fromFile', parse, compute,  applyMoves) where

import qualified Text.Megaparsec as M
import qualified Chess.Internal as Chess
import qualified PGN.Internal as PGN
import qualified PGN.Internal2 as PGN2
import qualified Chess.Internal2 as Chess2

type StringError = String

stringifyError :: Either PGN.ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return 

stringifyError2 :: Either PGN2.ParseError a -> Either StringError a
stringifyError2 = either (Left . M.errorBundlePretty) return

fromFile' :: String -> IO [String]
fromFile' = PGN.fromPGNFile' 

fromFile :: String -> IO (Either StringError [PGN.Game])
fromFile = fmap stringifyError . PGN.fromPGNFile

parse :: String -> Either StringError PGN.Game
parse = stringifyError . PGN.parseGame

compute :: String -> Either StringError Chess2.Board
compute = stringifyError2 . PGN2.parseCompute

computeMany :: String -> Either StringError [Chess2.Board]
computeMany = sequence . fmap compute . PGN2.fromString'


-- compute :: String -> Either StringError Chess.Board
-- compute = stringifyError . PGN.parseCompute

-- computeMany :: String -> Either StringError [Chess.Board]
-- computeMany = sequence . fmap compute . PGN.fromString'

applyMoves :: PGN.Game -> Either Chess.Outcome Chess.Board
applyMoves = PGN.runGame