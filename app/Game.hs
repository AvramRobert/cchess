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

data GameState = Play          |
                 Move C.Move   |
                 End C.Outcome | 
                 OfferDraw     |
                 Resign        |
                 Exit
                 deriving (Ord, Show, Eq)

instance M.ShowErrorComponent GameError where
    showErrorComponent (UnknownCommand input) = "Command of: " <> input <> " does not exist"

newP :: P.Parser GameState
newP = MC.string' "new game" $> Play

resignP :: P.Parser GameState
resignP = M.choice [MC.string' "resign", MC.string' "exit"] $> Resign

exitP :: P.Parser GameState
exitP = M.choice [MC.string' "exit", MC.string' "quit"] $> Exit

-- My PGN parser for pieces is case sensitive. It shouldn't be 
moveP :: Game -> P.Parser GameState 
moveP game = fmap Move $ P.moveParser (board game)

transitions :: Game -> GameState -> P.Parser GameState
transitions game Play     = moveP game <|> resignP
transitions _ _           = (M.many MC.asciiChar) $> Play

showBoard :: Game -> String
showBoard = D.gameBoard . board

prints :: GameState -> Game -> String
prints Play game = unlines $ [showBoard game, "Input a move"]
prints Exit game = "You son of a"

menuP :: P.Parser Game
menuP = MC.string' "new game" $> (Game { board = C.board, white = "Donney", black = "Larry" })

-- non-exhaustive
makeMove :: Game -> C.Move -> (GameState, Game)
makeMove game move = case (C.performEval (board game) move) of
    (Left (C.Checkmate x)) -> (End $ C.Checkmate x, game) -- this board doesn't contain the checkmating move
    (Left (C.Stalemate))   -> (End C.Stalemate, game)
    (Right board)          -> (Play, game { board = board })

recurse :: (GameState, Game) -> IO ()
recurse (state, game) = run state game

perform :: GameState -> Game -> IO ()
perform (Move m) game = recurse $ makeMove game m
perfrom (Resign) game = return ()

run :: GameState -> Game -> IO ()
run state game = do
    _      <- putStrLn $ prints state game
    input  <- getLine
    let parser = transitions game state
    case (P.run parser input) of (Right result) -> perform result game
                                 (Left err)     -> putStrLn "NOPE" 

menu = unlines ["Welcome to cchess!",
                "",
                "What do you want to do?",
                "- New Game", 
                "- Exit"]

-- 'runGame` and `run`, in this form, can actually be merged, can't they?
-- If I make the game part of the states that need it, I can merge them
runGame :: IO ()
runGame = do
    _ <- putStrLn menu
    i <- getLine
    case (P.run menuP i) of (Right game) -> run Play game
                            (Left err)   -> putStrLn "Shit"