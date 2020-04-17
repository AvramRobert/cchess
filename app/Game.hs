module Game (runGame) where

import Control.Monad (foldM)
import Control.Applicative ((<|>))
import qualified Chess.Display as D
import qualified Chess.Internal as C
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified PGN.Internal as P
import Data.Functor (($>))

data Game = Game { board :: C.Board,
                   white :: String,
                   black :: String }
            deriving (Eq, Show, Ord)

data State = Menu               |
             Play Game          |
             Resign Game        |
             End Game C.Outcome |
             Exit 
               
data Instruction = Display String                   | 
                   Interact (P.Parser Instruction)  | 
                   Transition State                 

showBoard :: Game -> String
showBoard = D.showBoard D.GameMode . board

showFigure :: C.Figure -> String
showFigure = D.showFigure D.GameMode

showOutcome :: C.Outcome -> String
showOutcome = D.showOutcome D.GameMode

menuText :: String
menuText = unlines ["Welcome to cchess!",
                    "",
                    "What do you want to do?",
                    "- New Game", 
                    "- Exit"]

playText :: Game -> String
playText game = unlines [showBoard game, "Input a move"]

outcomeText :: C.Outcome -> String
outcomeText (C.Checkmate _) = "You've been checkmated"
outcomeText (C.Stalemate _) = "This game is a stalemate"
outcomeText (C.Draw _)      = "This game is a draw. It cannot be won."
outcomeText (C.Illegal m)   =  show m <> " is illegal."

resignText :: C.Colour -> String
resignText c = unlines ["", "Result: (W) " <> w <> " -" <> b <> " (B)"]
    where w = if (c == C.W) then "1" else "0"
          b = if (c == C.B) then "1" else "0"

exitText :: String
exitText = "One day at a time."

newGameParser :: P.Parser Instruction
newGameParser = MC.string' "new game" $> (Transition $ Play Game { board = C.board, white = "Whitney", black = "Clareance" })

moveParser :: Game -> P.Parser Instruction
moveParser game = fmap (Transition . handle . C.performEval (board game)) $ P.moveParser (board game)
    where handle (Right board)  = Play game { board = board }
          handle (Left outcome) = End game outcome

exitGameParser :: P.Parser Instruction
exitGameParser = M.choice [ MC.string' "exit", MC.string' "quit" ] $> (Transition Exit)

resignParser :: Game -> P.Parser Instruction
resignParser game = M.choice [ MC.string' "resign", MC.string' "exit"] $> (Transition (Resign game))

instructions :: State -> [Instruction]
instructions (Menu)              = [Display menuText, Interact (newGameParser <|> exitGameParser)]
instructions (Exit)              = [Display exitText]
instructions (Play game)         = [Display (playText game), Interact (moveParser game <|> resignParser game)]
instructions (End game outcome)  = [Display (outcomeText outcome)]
instructions (Resign game)       = [Display (resignText $ C.player $ board game)]

process :: Instruction -> IO ()
process (Display text)     = putStrLn text
process (Transition state) = run state 
process (Interact p)       = getLine >>= (handle . P.run p)
    where handle (Right i)            = process i
          handle (Left err)           = maybe (unknown err) known $ P.chessError err
          unknown err                 = process (Display $ "Unknown input. Try again\n") >> process (Interact p)  
          known (P.MissingMovesError) = process (Display $ "Unknown move. Try again") >> process (Interact p)
          known (P.CaptureError c f)  = process (Display $ "Cannot capture with " <> showFigure f <> " at " <> show c) >> process (Interact p) 
          known (P.AdvanceError c f)  = process (Display $ "Cannot advance to " <> showFigure f <> " with " <> show pi) >> process (Interact p)
          known (P.PromoteError c f)  = process (Display $ "Cannot promote to " <> showFigure f <> " at " <> show c) >> process (Interact p)
          known (P.CastleError c)     = process (Display $ "Cannot castle" <> show c) >> process (Interact p)
          known (P.GameError o)       = process (Display $ "Cannot perform because the game is: " <> show o) >> process (Interact p)

run :: State -> IO ()
run = foldM (\_ i -> process i) () . instructions

runGame :: IO ()
runGame = run Menu