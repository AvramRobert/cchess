{-# LANGUAGE GADTs #-}

module Game (runGame) where

import qualified Chess.Display as D
import qualified Chess.Internal as C
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified PGN.Internal as P
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Lib.Freer
import Chess.Meta

data State = Menu               |
             Play Game          |
             Resign Game        |
             End Game C.Outcome |
             Exit 

data Prompt a where
    Display :: String     -> Prompt String
    Input   :: P.Parser a -> Prompt a
    Stop    :: Prompt a

type Instruction a = Freer Prompt a

display :: String -> Instruction String
display = perform . Display

input :: P.Parser State -> Instruction State
input = perform . Input

stop :: Instruction State
stop = perform Stop

showBoard :: Game -> String
showBoard = D.showBoard D.GameMode . board

showFigure :: C.Figure -> String
showFigure = D.showFigure D.GameMode

showOutcome :: C.Outcome -> String
showOutcome = D.showOutcome D.GameMode

showCastles :: C.Castles -> String
showCastles = D.showCastles D.GameMode

menuText :: String
menuText = unlines ["Welcome to cchess!",
                    "",
                    "What do you want to do?",
                    "- New Game", 
                    "- Exit"]

playText :: Game -> String
playText game = unlines [showBoard game, "Input a move"]

-- I could re-add the suggestion that a game is drawn/drawable
outcomeText :: C.Outcome -> String
outcomeText (C.Checkmate) = "You've been checkmated"
outcomeText (C.Stalemate) = "This game is a stalemate"
outcomeText (C.Illegal m) =  show m <> " is illegal"

resignText :: C.Colour -> String
resignText c = unlines ["", "Result: (W) " <> w <> " - " <> b <> " (B)"]
    where w = if (c == C.W) then "1" else "0"
          b = if (c == C.B) then "1" else "0"

exitText :: String
exitText = "One day at a time."

newGame :: P.Parser State
newGame = MC.string' "new game" $> Play Game { board = C.board, tags = [] }

move :: Game -> P.Parser State
move game = fmap (handle . C.performEval (board game)) $ P.moveParser (board game)
    where handle (Right board)  = Play game { board = board }
          handle (Left outcome) = End game outcome

exit :: P.Parser State
exit = M.choice [ MC.string' "exit", MC.string' "quit" ] $> Exit

resign :: Game -> P.Parser State
resign game = M.choice [ MC.string' "resign", MC.string' "exit"] $> (Resign game)

instructions :: State -> Instruction State
instructions (Menu)              = display menuText >> input (newGame <|> exit)
instructions (Play game)         = display (playText game) >> input (move game <|> resign game)
instructions (Exit)              = display exitText >> stop
instructions (End game outcome)  = display (outcomeText outcome) >> stop
instructions (Resign game)       = display (resignText $ C.player $ board game) >> stop

process :: Instruction State -> IO ()
process (Value s)                     = run s
process (Effect (Stop) _)             = return ()
process (Effect (Display s) f)        = putStrLn s >> (process $ f s)
process eff @ (Effect (Input p) f)    = getLine >>= (handle . P.run p)
    where handle (Right state)        = process $ f state
          handle (Left error)         = maybe (unknown error) (known) (P.chessError error) >> process eff
          unknown error               = putStrLn $ "Unknown input. Try again\n"
          known (P.MissingMovesError) = putStrLn $ "Unknown move. Try again"
          known (P.CaptureError c f)  = putStrLn $ "Cannot capture with " <> showFigure f <> " at " <> show c 
          known (P.AdvanceError c f)  = putStrLn $ "Cannot advance with " <> showFigure f <> " to " <> show c 
          known (P.PromoteError c f)  = putStrLn $ "Cannot promote to " <> showFigure f <> " at " <> show c
          known (P.CastleError c)     = putStrLn $ "Cannot castle " <> showCastles c
          known (P.GameError o)       = putStrLn $ "Cannot perform because the game is: " <> showOutcome o

run :: State -> IO ()
run = process . instructions

runGame :: IO ()
runGame = run Menu