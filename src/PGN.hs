module PGN (fromFile, fromFile', parseGame, parseManyGames, parseMove, writeGame, writeMove) where

import qualified Text.Megaparsec as M
import qualified Chess as C
import qualified Chess.Game as G
import qualified PGN.Parser as P
import qualified PGN.Writer as W

type StringError = String

stringifyError :: Either P.ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return

fromFile' :: String -> IO [String]
fromFile' = P.fromPGNFile' 

fromFile :: String -> IO (Either StringError [G.Game])
fromFile = fmap stringifyError . P.fromPGNFile

parseGame :: String -> Either StringError G.Game
parseGame = stringifyError . P.parseGame

parseManyGames :: String -> Either StringError [G.Game]
parseManyGames = sequence . fmap parseGame . P.fromString'

parseMove :: String -> G.Game -> Either StringError C.Move
parseMove move = stringifyError . P.parseMove move . G.board

writeGame :: G.Game -> String
writeGame = unlines . W.writeMoves . G.board

writeMove :: C.Move -> G.Game -> Maybe String
writeMove move = W.writeMove move . G.board