module Game (runGame) where

import qualified Chess.Display as D
import qualified Chess.Internal as C
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified PGN.Internal as P
import Data.Functor (($>))

data Interactions a = PrintLine String      |
                      ReadLine (P.Parser a) 

type GameParser a = M.Parsec GameError String a

data Game = Game { board :: C.Board,
                   white :: String,
                   black :: String }
            deriving (Eq, Show, Ord)

data GameError = UnknownCommand String 
                 deriving (Show, Eq, Ord)

data GameState = New                 | 
                 Current Game             |
                 Move Game C.Move         | 
                 GiveUp Game C.Outcome    |
                 OfferDraw Game C.Outcome
                 deriving (Ord, Show, Eq)

instance M.ShowErrorComponent GameError where
    showErrorComponent (UnknownCommand input) = "Command of: " <> input <> " does not exist"

createGame :: P.Parser GameState
createGame = return $ Current $ Game { board = C.board, white = "player1", black = "player2" }

-- the move parser is case-sensitive. should it be?
inputMove :: Game -> P.Parser GameState 
inputMove game = (P.moveParser $ board game) >>= (handle . perform)
    where perform          = C.performEval (board game)
          handle (Right b) = return $ Current $ game { board = b }
          handle (Left  o) = P.failWith (P.GameError o) Nothing 

giveUp :: Game -> P.Parser GameState
giveUp game = M.choice [M.try $ MC.string' "give up", 
                        M.try $ MC.string' "forfeit", 
                        M.try $ MC.string' "stop"] $> (GiveUp game $ C.Forfeit loser) 
    where loser = C.player $ board game

machine :: GameState -> P.Parser GameState
machine New            = createGame 
machine (Current game) = M.choice [M.try $ inputMove game, M.try $ giveUp game]

newGameParser :: GameParser GameState
newGameParser = MC.string' "new game" $> New

showTurn :: Game -> String
showTurn game = "It's " <> (show $ C.player $ board game) <>"'s turn: " -- Chess.Colou needs a DebugMode/GameMode show of it's own

showBoard :: Game -> String
showBoard = D.gameBoard . board

anyString :: P.Parser String
anyString = M.many MC.asciiChar

matches :: (GameState, [Interactions String])
matches = (New, [PrintLine "New Game!", 
                 PrintLine "Please input name for white:", 
                 ReadLine anyString,
                 PrintLine "Please input name for black:",
                 ReadLine anyString])

-- the pattern that i would like is to have every individual game state as an ADT and every one of these associated with a function that 
-- can consume it's input and propell the game to the next state
-- this preferably being another member of the same ADT

-- better still, every pairing should have an additional association with 

-- My game is happening in totality at the text level
-- I could define a typeclass that shows me how to textualise every parser outcome for the game
start :: GameState -> IO ()
start New                    = start (Current $ Game { board = C.board, white = "white", black = "black"} )
start (GiveUp game outcome)  = putStrLn $ show outcome
start state @ (Current game) = do
    _     <- putStrLn $ showBoard game
    _     <- putStrLn $ showTurn game
    input <- getLine
    case (P.run (machine state) input) of
        (Right state') -> start state'
        (Left e)       -> do putStrLn "You may want to try that again"
                             start state

runGame :: IO ()
runGame = do
    _     <- putStrLn "What do you want to do?"
    input <- getLine
    case (P.run newGameParser input) of
        (Right state) -> start state
        (Left err)    -> return () 