module Chess.Game where

import qualified Chess.Internal as Chess
import Chess.Display

data Game = Game { tags  :: [Tag],
                   board :: Chess.Board }

data Outcome = Win Chess.Colour 
             | Draw 
             | Other deriving (Show, Ord, Eq)

data Reason  = Abandoned 
             | Adjundication 
             | Death 
             | Emergency 
             | Checkmate
             | Resignation
             | Stalemate 
             | Infraction 
             | TimeForfeit 
             | Unterminated
             deriving (Show, Eq)

data Variant = OTB | ICS deriving (Show, Eq) -- Over The Board | Internet Chess Server

-- http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
-- look here to find all possible headers
data Tag = Event String
          | Site String
          | Date String
          | Round String
          | White String
          | Black String
          | Result Outcome
             -- These are optional
          | EventDate String
          | ECO String
          | WhiteElo String
          | BlackElo String
          | PlyCount String
          | Annotator String
          | TimeControl String
          | Time String
          | Termination Reason
          | Mode Variant
          | FEN String -- Initial position on the board in Forsyth-Edwards Notation
          | Unknown String String
          deriving (Show, Eq)

instance Ord Tag where
    compare a b = (tagRank a) `compare` (tagRank b)

tagRank :: Tag -> Int
tagRank rank = case rank of 
    (Event _)    -> 1
    (Site _)     -> 2
    (Date _)     -> 3
    (Round _)    -> 4
    (White _)    -> 5
    (Black _)    -> 6
    (Result _)   -> 7
    (WhiteElo _) -> 8
    (BlackElo _) -> 9
    (ECO _)      -> 10
    _            -> 11

-- Implement this checkmate and all
evaluate :: Chess.Board -> Maybe Reason
evaluate board = let immoble = Chess.immoble board
                     checked = Chess.check board
                 in if (checked && immoble) then Just Checkmate
                    else if immoble         then Just Stalemate
                    else                         Nothing