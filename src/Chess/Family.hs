{-# LANGUAGE GADTs, RankNTypes #-}

module Chess.Family where

import qualified Chess.Internal as Chess

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

newtype Event = Event String deriving (Show)
newtype Site = Site String deriving (Show)
newtype Date = Date String
newtype Round = Round String
newtype White = White String
newtype Black = Black String
newtype Result = Result Outcome

newtype WhiteElo = WhiteElo Rating
newtype BlackElo = BlackElo Rating
newtype WhiteTitle = WhiteTitle Title
newtype BlackTitle = BlackTitle Title
newtype WhiteUSCF = WhiteUSCF String
newtype BlackUSCF = BlackUSCF String
newtype WhiteNA = WhiteNA Address
newtype BlackNA = BlackNA Address
newtype WhiteType = WhiteType PlayerType
newtype BlackType = BlackType PlayerType
newtype EventDate = EventDate String
newtype EventSponsor = EventSponsor String
newtype Section = Section String
newtype Stage = Stage String
newtype Board = Board String 

newtype Opening = Opening String
newtype Variation = Variantion String
newtype SubVariation = SubVariation String

newtype ECO = ECO String
newtype NIC = NIC String
newtype Time = Time String
newtype UTCTime = UTCTime String
newtype UTCDate = UTCDate String
newtype TimeControl = TimeControl String

newtype SetUp = SetUp String
newtype FEN = FEN String

newtype Termination = Termination Reason
newtype PlyCount = PlyCount String
newtype Annotator = Annotator String
newtype Mode = Mode Variant
newtype Unknown = Unknown (String, String)

data Tag a where
  TEvent :: Tag Event
  TSite :: Tag Site

data HTag where
  HTag :: Tag a -> a -> HTag 

class Tagged a where
  tag :: Tag a

data Game = Game { tags :: [HTag], board :: Chess.Board }

instance Tagged Event where tag = TEvent
instance Tagged Site where tag = TSite

unwrap :: Tag a -> HTag -> Maybe a
unwrap TEvent (HTag TEvent e) = Just e
unwrap TSite  (HTag TSite e)  = Just e

locate :: Tag a -> [HTag] -> Maybe a
locate tag ([])     = Nothing
locate tag (h : hs) = case (unwrap tag h) of 
  (Just a)  -> Just a
  (Nothing) -> locate tag hs

hide :: Tagged a => a -> HTag
hide = HTag tag

add :: Tagged a => a -> Game -> Game
add a (Game hs b) = Game ((hide a) : hs) b 

getEvent = locate TEvent
getSite = locate TSite

newGame :: Event -> Site -> Game
newGame e s = Game { tags = [hide e, hide s], board = Chess.emptyBoard }