module Chess.Meta where

import qualified Chess.Internal as Chess
import Chess.Display

data Game = Game { tags  :: [Tag],
                   board :: Chess.Board }

data GameResult = Win Chess.Colour | Draw | Other deriving (Show, Ord, Eq)

-- http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
-- look here to find all possible headers
-- I can make a generic that that just captures the entry name and its content, instead of Just ignoring them
data Tag = Event String
          | Site String
          | Date String
          | Round String
          | White String
          | Black String
          | Result GameResult -- It can have three instances Win Colour (1-0, 0-1), Draw (1/2-1/2), Other (*) (Game is ongoing)
             -- These are optional
          | EventDate String
          | ECO String
          | WhiteElo String
          | BlackElo String
          | PlyCount String
          | Annotator String
          | TimeControl String
          | Time String
          | Termination String -- OUTCOME! -> Values (Abandoned, Adjundication, Death, Emergency, Normal, Rules Infraction, Time Forfeit, Unterminated)
          | Mode String -- Can be: OTB (Over the board), ICS (Internet chess server)
          | FEN String -- Initial position on the board in Forsyth-Edwards Notation
          | Ignored
          deriving (Show, Eq)

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

instance Ord Tag where
    compare a b = (tagRank a) `compare` (tagRank b)