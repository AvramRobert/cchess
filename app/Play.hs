{-# LANGUAGE GADTs #-}

module Play (playGame) where

import qualified Chess.Display as D
import qualified Chess.Game    as G
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Data.Set as S
import qualified Chess as C
import qualified Control.Applicative as A
import Control.Applicative (Alternative, (<|>))
import Data.Functor (($>))
import Lib.Megaparsec (customError)
import Lib.Freer

-- this thing can actually define its own errors
newtype Parser a = Parser (M.Parsec C.Error String a)

data State = Menu                
           | Play   G.Game
           | Fen    G.Game
           | Resign G.Game
           | End    G.Game G.Reason   
           | Exit 

data Prompt a where
    Display :: String   -> Prompt String
    Input   :: Parser a -> Prompt a
    Stop    :: Prompt a

type Instruction a = Freer Prompt a

display :: String -> Instruction String
display = perform . Display

input :: Parser State -> Instruction State
input = perform . Input

stop :: Instruction State
stop = perform Stop

instance Functor Parser where 
    fmap f (Parser p) = Parser (fmap f p)

instance Applicative Parser where 
    pure = Parser . pure
    (<*>) (Parser f) (Parser a) = Parser (f <*> a)
           
instance Monad Parser where 
    (>>=) (Parser p) f = Parser (p >>= (extract . f))
            where extract (Parser p) = p

instance Alternative Parser where
    empty = Parser A.empty
    (<|>) (Parser a) (Parser b) = Parser (a <|> b)

instance C.ParserTie Parser where
    getInput  = Parser   M.getInput
    setInput  = Parser . M.setInput
    failWith  = Parser . M.fancyFailure . S.fromList . return . M.ErrorCustom 

runParser :: Parser a -> String -> Either C.Error a
runParser (Parser p) = either (Left . makeError) Right . M.runParser p ""
    where makeError    = customError id (const unknownInput)
          unknownInput = C.Error C.InputError "Unknown input"

menuText :: String
menuText = unlines ["Welcome to cchess!",
                    "",
                    "What do you want to do?",
                    "- New Game", 
                    "- Exit"]

boardText :: G.Game -> String
boardText = D.showGameBoard D.GameMode

playText :: String
playText = unlines ["", "Input a move"]

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

fenText :: G.Game -> String
fenText game = unlines ["", C.writeFen game]

newGame :: Parser State
newGame = Parser (MC.string' "new game" $> (Play C.quickGame))

move :: G.Game -> Parser State
move = fmap transition . C.evaluatedMoveParser
    where transition (C.Continue game)    = Play game
          transition (C.Retry game)       = Play game 
          transition (C.Terminate game r) = End  game r  

exit :: Parser State
exit = Parser (M.choice [ MC.string' "exit", MC.string' "quit" ] $> Exit)

resign :: G.Game -> Parser State
resign game = Parser (M.choice [ MC.string' "resign", MC.string' "exit", MC.string' "quit" ] $> (Resign game))

fen :: G.Game -> Parser State
fen game = Parser (MC.string' "fen" ) $> (Fen game)

instructions :: State -> Instruction State
instructions (Menu)              = display menuText >> input (newGame <|> exit)
instructions (Play game)         = display (boardText game) >> display playText >> input (move game <|> fen game <|> resign game)
instructions (Fen game)          = display (fenText game) >> display playText >> input (move game <|> fen game <|>  resign game)
instructions (Exit)              = display exitText >> stop
instructions (End game outcome)  = display (outcomeText outcome) >> stop
instructions (Resign game)       = display (resignText $ C.currentPlayer game) >> stop

process :: Instruction State -> IO ()
process (Value s)                     = run s
process (Effect (Stop) _)             = return ()
process (Effect (Display s) f)        = putStrLn s >> (process $ f s)
process eff @ (Effect (Input p) f)    = getLine >>= (handle . runParser p)
    where handle (Right state)        = process (f state)
          handle (Left error)         = putStrLn (C.message error) >> process eff

run :: State -> IO ()
run = process . instructions

playGame :: IO ()
playGame = run Menu