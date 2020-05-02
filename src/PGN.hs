module PGN (fromFile, fromFile', parseGame, parseManyGames, parseMove, write) where

import qualified Text.Megaparsec as M
import Chess.Game (Game)
import Chess.Internal (Move, Board)
import PGN.Parser (ParseError)
import qualified Chess.Game as G
import qualified PGN.Parser as P
import qualified PGN.Writer as W

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

write :: Game -> String
write = unlines . W.writeMoves . G.board

parseMove :: String -> Board -> Either ParseError Move
parseMove move = P.parseMove move