{-# LANGUAGE GADTs #-}

module Chess.Game where

import qualified Chess.Internal as Chess

data Game = Game { tags  :: [Tag],
                   board :: Chess.Board }

data Outcome = Win Chess.Colour 
             | Draw 
             | Other deriving (Ord, Eq)

data Reason  = Abandoned 
             | Adjundication 
             | Death 
             | Emergency 
             | Normal
             | Checkmate   --
             | Resignation -- these three pertain actually to 'normal'. I've put them here myself because they are sort-of undocumented
             | Stalemate   -- 
             | Infraction 
             | TimeForfeit 
             | Unterminated
             deriving (Show, Eq)

data Variant = OTB | ICS deriving (Show, Eq) -- Over The Board | Internet Chess Server

data Title = GM | FM | IM | UT deriving (Show, Eq) -- Grandmaster, FIDE Master, International Master, Untitled

data Rating = Rated String | Unrated deriving (Show, Eq)

data Address = Address String | NoAddress deriving (Show, Eq)

data PlayerType = Human | Computer deriving (Show, Eq)

-- use GADTs to describe these
-- http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
data Tag =  Event String
          | Site String
          | Date String
          | Round String
          | White String
          | Black String
          | Result Outcome
             -- These are optional
            -- Player
          | WhiteElo Rating
          | BlackElo Rating
          | WhiteTitle Title
          | BlackTitle Title
          | WhiteUSCF String
          | BlackUSCF String
          | WhiteNA Address
          | BlackNA Address
          | WhiteType PlayerType
          | BlackType PlayerType
            -- Event
          | EventDate String
          | EventSponsor String
          | Section String
          | Stage String
          | Board String
            -- Opening (locale)
          | Opening String
          | Variation String
          | SubVariation String
            -- Opening (third-party)
          | ECO String
          | NIC String
            -- Time 
          | Time String         --  
          | UTCTime String      -- go down the rabbit whole and try to parse these propely?
          | UTCDate String      --
          | TimeControl String -- I'm not sure about this, it seems a bit iffy in its definition
           -- Alternative starting positions
          | SetUp String
          | FEN String
            -- Game conclusion
          | Termination Reason
            -- Misc
          | PlyCount String
          | Annotator String  
          | Mode Variant
          | Unknown String String
          deriving (Eq)

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
    _            -> 8