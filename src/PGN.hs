module PGN (fromFile, fromFile', parseGame, parseManyGames, parseMove, writeGame, writeMove) where

import qualified Text.Megaparsec as M
import Chess.Game (Game)
import PGN.Parser (ParseError)
import qualified Chess.Game as G
import qualified PGN.Parser as P
import qualified PGN.Writer as W
import Chess (Board, Move)

type StringError = String

stringifyError :: Either ParseError a -> Either StringError a
stringifyError = either (Left . M.errorBundlePretty) return

fromFile' :: String -> IO [String]
fromFile' = P.fromPGNFile' 

fromFile :: String -> IO (Either StringError [Game])
fromFile = fmap stringifyError . P.fromPGNFile

parseGame :: String -> Either StringError Game
parseGame = stringifyError . P.parseGame

parseManyGames :: String -> Either StringError [Game]
parseManyGames = sequence . fmap parseGame . P.fromString'

parseMove :: String -> Game -> Either ParseError Move
parseMove move = P.parseMove move . G.board

writeGame :: Game -> String
writeGame = unlines . W.writeMoves . G.board

writeMove :: Move -> Game -> String
writeMove move = W.writeMove move . G.board