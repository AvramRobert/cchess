module Game (runGame) where

import Control.Applicative ((<|>))
import qualified Chess.Display as D
import qualified Chess.Internal as C
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified PGN.Internal as P
import Data.Functor (($>))

type GameParser a = M.Parsec GameError String a

type Instruction a = M.ParsecT GameError String IO a

data Game = Game { board :: C.Board,
                   white :: String,
                   black :: String }
            deriving (Eq, Show, Ord)

data GameError = UnknownCommand String 
                 deriving (Show, Eq, Ord)

data GameState = Menu               |
                 Play Game          |
                 Move Game C.Move   |
                 End Game C.Outcome | 
                 OfferDraw          |
                 Resign             |
                 Exit
                 deriving (Ord, Show, Eq)

instance M.ShowErrorComponent GameError where
    showErrorComponent (UnknownCommand input) = "Command of: " <> input <> " does not exist"

showBoard :: Game -> String
showBoard = D.gameBoard . board

newP :: P.Parser GameState
newP = MC.string' "new game" $> (Play $ Game { board = C.board, white = "Donney", black = "Larry" })

resignP :: P.Parser GameState
resignP = M.choice [MC.string' "resign", MC.string' "exit"] $> Resign

exitP :: P.Parser GameState
exitP = M.choice [MC.string' "exit", MC.string' "quit"] $> Exit

moveP :: Game -> P.Parser GameState 
moveP game = fmap (Move game) $ P.moveParser (board game)

menuT = unlines ["Welcome to cchess!",
                 "",
                 "What do you want to do?",
                 "- New Game", 
                 "- Exit"]

playT game = unlines [showBoard game, "Input a move"]

exitT = "One day at a time."

transitions :: GameState -> P.Parser GameState
transitions Menu        = newP <|> exitP 
transitions (Play game) = moveP game <|> resignP
transitions _           = (M.many MC.asciiChar) $> Exit

texts :: GameState -> String
texts Menu        = menuT 
texts Exit        = exitT
texts (Play game) = playT game

-- non-exhaustive
makeMove :: Game -> C.Move -> GameState
makeMove game move = case (C.performEval (board game) move) of
    (Left (C.Checkmate x)) -> End game $ C.Checkmate x -- this board doesn't contain the checkmating move
    (Left (C.Stalemate))   -> End game C.Stalemate 
    (Right board)          -> Play $ game { board = board }

perform :: GameState -> IO ()
perform (Play game)   = run $ (Play game)
perform (Move game m) = run $ makeMove game m
perfrom (Resign)      = return ()

handle :: GameState -> P.ChessError -> IO ()
handle state P.MissingMovesError = putStrLn "Unknown move. Try again" >> run state

run :: GameState -> IO ()
run state = do
    let text   = texts state
    let parser = transitions state
    _          <- putStrLn text
    input      <- getLine -- these should be escaped
    case (P.run parser input) of 
        (Right state') -> perform state'
        (Left err)     -> maybe (putStrLn $ unlines ["Input error", M.errorBundlePretty err]) (handle state) $ P.chessError err

runGame :: IO ()
runGame = run Menu