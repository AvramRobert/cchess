{-# LANGUAGE GADTs #-}

module Play (playGame) where

import qualified Chess.Display as D
import qualified Chess.Game    as G
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Chess as C
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Lib.Freer

data State = Menu                
           | Play   G.Game         
           | Resign G.Game       
           | End    G.Game G.Reason   
           | Exit 

data Prompt a where
    Display :: String     -> Prompt String
    Input   :: C.Parser a -> Prompt a
    Stop    :: Prompt a

type Instruction a = Freer Prompt a

display :: String -> Instruction String
display = perform . Display

input :: C.Parser State -> Instruction State
input = perform . Input

stop :: Instruction State
stop = perform Stop

menuText :: String
menuText = unlines ["Welcome to cchess!",
                    "",
                    "What do you want to do?",
                    "- New Game", 
                    "- Exit"]

playText :: G.Game -> String
playText game = unlines [D.showGameBoard D.GameMode game, "Input a move"]

-- I could re-add the suggestion that a game is drawn/drawable
outcomeText :: G.Reason  -> String
outcomeText (G.Checkmate) = "You've been checkmated"
outcomeText (G.Stalemate) = "This game is a stalemate"

resignText :: C.Colour -> String
resignText c = unlines ["", "Result: (W) " <> w <> " - " <> b <> " (B)"]
    where w = if (c == C.W) then "1" else "0"
          b = if (c == C.B) then "1" else "0"

exitText :: String
exitText = "One day at a time."

newGame :: C.Parser State
newGame = MC.string' "new game" $> (Play $ C.newGame "Whitney" "Clareance")

move :: G.Game -> C.Parser State
move = fmap transition . C.evaluatedMoveParser
    where transition (C.Continue game)    = Play game
          transition (C.Retry game)       = Play game 
          transition (C.Terminate game r) = End  game r  

exit :: C.Parser State
exit = M.choice [ MC.string' "exit", MC.string' "quit" ] $> Exit

resign :: G.Game -> C.Parser State
resign game = M.choice [ MC.string' "resign", MC.string' "exit"] $> (Resign game)

instructions :: State -> Instruction State
instructions (Menu)              = display menuText >> input (newGame <|> exit)
instructions (Play game)         = display (playText game) >> input (move game <|> resign game)
instructions (Exit)              = display exitText >> stop
instructions (End game outcome)  = display (outcomeText outcome) >> stop
instructions (Resign game)       = display (resignText $ C.currentPlayer game) >> stop

process :: Instruction State -> IO ()
process (Value s)                     = run s
process (Effect (Stop) _)             = return ()
process (Effect (Display s) f)        = putStrLn s >> (process $ f s)
process eff @ (Effect (Input p) f)    = getLine >>= (handle . C.runParser p)
    where handle (Right state)        = process (f state)
          handle (Left error)         = putStrLn (C.message error) >> process eff

run :: State -> IO ()
run = process . instructions

playGame :: IO ()
playGame = run Menu