module Game (runGame) where

import qualified Chess.Display as D
import qualified Chess.Internal as C
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified PGN.Internal as P
import Data.Functor (($>))

type GameParser a = M.Parsec GameError String a

data Game = Game { board :: C.Board,
                   white :: String,
                   black :: String }
            deriving (Eq, Show, Ord)

data GameError = UnknownCommand String 
                 deriving (Show, Eq, Ord)

data GameState = New                      | 
                 Current Game             |
                 Move Game C.Move         | 
                 GiveUp Game C.Outcome    |
                 OfferDraw Game C.Outcome
                 deriving (Ord, Show, Eq)

instance M.ShowErrorComponent GameError where
    showErrorComponent (UnknownCommand input) = "Command of: " <> input <> " does not exist"

createGame :: P.Parser GameState
createGame = return $ Current $ Game { board = C.board, white = "player1", black = "player2" }

inputMove :: Game -> P.Parser GameState 
inputMove game = (P.moveParser $ board game) >>= (handle . perform)
    where perform          = C.performEval (board game)
          handle (Right b) = return $ Current $ game { board = b }
          handle (Left  o) = P.failWith (P.GameError o) Nothing 

giveUp :: Game -> P.Parser GameState
giveUp game = return $ GiveUp game $ C.Forfeit loser
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

-- My game is happening in totality at the text level
-- I could define a typeclass that shows me how to textualise every parser outcome for the game
start :: GameState -> IO ()
start New                    = start (Current $ Game { board = C.board, white = "white", black = "black"} )
start (GiveUp game outcome)  = putStrLn $ show outcome
start state @ (Current game) = do
    _     <- putStrLn $ showBoard game
    _     <- putStrLn $ showTurn game
    input <- getLine
    case (P.run (machine state) input) of -- interestigly, if nothing matches in the machine parser alternatives, it takes the last parser and assumes it's correct
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