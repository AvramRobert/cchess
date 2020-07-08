{-# LANGUAGE GADTs #-}

module Chess.Game where

import qualified Chess.Internal as Chess

data Outcome = WhiteWin
             | BlackWin
             | Draw 
             | Other deriving (Show, Ord, Eq)

data Rating = Rated String | Unrated deriving (Show, Eq)

data Address = Address String | NoAddress deriving (Show, Eq)

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

data Title = GM | FM | IM | UT deriving (Show, Eq) -- Grandmaster, FIDE Master, International Master, Untitled

data PlayerType = Human | Computer deriving (Show, Eq)

data GameMode = OTB | ICS deriving (Show, Eq)

data Event
data Site
data Date
data Round
data White
data Black
data Result

data WhiteElo
data BlackElo
data WhiteTitle
data BlackTitle
data WhiteUSCF
data BlackUSCF
data WhiteNA
data BlackNA
data WhiteType
data BlackType
data EventDate
data EventSponsor
data Section
data Stage
data Board

data Opening
data Variation
data SubVariation

data ECO
data NIC
data Time
data UTCTime
data UTCDate
data TimeControl

data SetUp
data FEN

data Termination
data PlyCount
data Annotator
data Mode
data Unknown

data Tag a b where
  Event        :: Tag Event String
  Site         :: Tag Site String
  Date         :: Tag Date String
  Round        :: Tag Round String
  White        :: Tag White String
  Black        :: Tag Black String
  Result       :: Tag Result Outcome
  WhiteElo     :: Tag WhiteElo Rating
  BlackElo     :: Tag BlackElo Rating
  WhiteTitle   :: Tag WhiteTitle Title
  BlackTitle   :: Tag BlackTitle Title
  WhiteUSCF    :: Tag WhiteUSCF String
  BlackUSCF    :: Tag BlackUSCF String
  WhiteNA      :: Tag WhiteNA Address
  BlackNA      :: Tag BlackNA Address
  WhiteType    :: Tag WhiteType PlayerType
  BlackType    :: Tag BlackType PlayerType
  EventDate    :: Tag EventDate String
  EventSponsor :: Tag EventSponsor String
  Section      :: Tag Section String
  Stage        :: Tag Stage String
  Board        :: Tag Board String
  Opening      :: Tag Opening String
  Variation    :: Tag Variation String
  SubVariation :: Tag SubVariation String
  ECO          :: Tag ECO String
  NIC          :: Tag NIC String
  Time         :: Tag Time String
  UTCTime      :: Tag UTCTime String
  UTCDate      :: Tag UTCDate String
  TimeControl  :: Tag TimeControl String
  SetUp        :: Tag SetUp String
  FEN          :: Tag FEN String
  Termination  :: Tag Termination Reason
  PlyCount     :: Tag PlyCount String
  Annotator    :: Tag Annotator String
  Mode         :: Tag Mode GameMode
  Unknown      :: Tag Unknown (String, String)

data Entry a where
  Entry :: Tag a b -> b -> Entry a

data HEntry where
  HEntry :: Entry a -> HEntry

data Game = Game { entries   :: [HEntry], 
                   gameBoard :: Chess.Board }

instance Eq HEntry where
  (==) (HEntry this) (HEntry that) = equate this that

instance Ord HEntry where
  compare (HEntry a) (HEntry b) = (rank a) `compare` (rank b)
    where rank (Entry Event _)  = 1
          rank (Entry Site _)   = 2
          rank (Entry Date _)   = 3
          rank (Entry Round _)  = 4
          rank (Entry White _)  = 5
          rank (Entry Black _)  = 6
          rank (Entry Result _) = 7
          rank _                = 8

equality :: Tag a b -> b -> b -> Bool
equality Event        = (==)
equality Site         = (==)
equality Date         = (==)
equality Round        = (==)
equality White        = (==)
equality Black        = (==)
equality Result       = (==)
equality WhiteElo     = (==)
equality WhiteTitle   = (==)
equality BlackTitle   = (==)
equality WhiteUSCF    = (==)
equality BlackUSCF    = (==)
equality WhiteNA      = (==)
equality BlackNA      = (==)
equality WhiteType    = (==)
equality BlackType    = (==)
equality EventSponsor = (==)
equality Section      = (==)
equality Stage        = (==)
equality Board        = (==)
equality Opening      = (==)
equality Variation    = (==)
equality SubVariation = (==)
equality ECO          = (==)
equality NIC          = (==)
equality Time         = (==)
equality UTCTime      = (==)
equality UTCDate      = (==)
equality TimeControl  = (==)
equality SetUp        = (==)
equality FEN          = (==)
equality Termination  = (==)
equality PlyCount     = (==)
equality Annotator    = (==)
equality Mode         = (==)
equality Unknown      = (==)

equate :: Entry a -> Entry b -> Bool
equate (Entry tag value) that = maybe False (equals value) (match tag that)
  where equals = equality tag

match :: Tag a b -> Entry c -> Maybe b
match Event (Entry Event v)               = Just v
match Site  (Entry Site v)                = Just v
match Date  (Entry Date v)                = Just v
match Round (Entry Round v)               = Just v
match White (Entry White v)               = Just v
match Black (Entry Black v)               = Just v
match Result (Entry Result v)             = Just v
match WhiteElo (Entry WhiteElo v)         = Just v
match BlackElo (Entry BlackElo v)         = Just v
match WhiteTitle (Entry WhiteTitle v)     = Just v
match BlackTitle (Entry BlackTitle v)     = Just v
match WhiteUSCF (Entry WhiteUSCF v)       = Just v
match BlackUSCF (Entry BlackUSCF v)       = Just v
match WhiteNA (Entry WhiteNA v)           = Just v
match BlackNA (Entry BlackNA v)           = Just v
match WhiteType (Entry WhiteType v)       = Just v
match BlackType (Entry BlackType v)       = Just v
match EventSponsor (Entry EventSponsor v) = Just v
match Section (Entry Section v)           = Just v
match Stage (Entry Stage v)               = Just v
match Board (Entry Board v)               = Just v
match Opening (Entry Opening v)           = Just v
match Variation (Entry Variation v)       = Just v
match SubVariation (Entry SubVariation v) = Just v
match ECO (Entry ECO v)                   = Just v
match NIC (Entry NIC v)                   = Just v
match Time (Entry Time v)                 = Just v
match UTCTime (Entry UTCTime v)           = Just v
match UTCDate (Entry UTCDate v)           = Just v
match TimeControl (Entry TimeControl v)   = Just v
match SetUp (Entry SetUp v)               = Just v
match FEN (Entry FEN v)                   = Just v
match Termination (Entry Termination v)   = Just v
match PlyCount (Entry PlyCount v)         = Just v
match Annotator (Entry Annotator v)       = Just v
match Mode (Entry Mode v)                 = Just v
match Unknown (Entry Unknown v)           = Just v
match _ _                                 = Nothing

determine :: Tag a b -> [HEntry] -> Maybe b
determine _ []                    = Nothing
determine tag ((HEntry entry):es) = maybe (determine tag es) Just (match tag entry)

locate :: Tag a b -> Game -> Maybe b
locate tag (Game entries board) = determine tag entries

tag :: Entry a -> Game -> Game
tag entry (Game entries board) = Game { entries = (HEntry entry) : entries, gameBoard = board }
 
event        = Entry Event
site         = Entry Site
date         = Entry Date
round        = Entry Round
white        = Entry White
black        = Entry Black
result       = Entry Result
whiteElo     = Entry WhiteElo
blackElo     = Entry BlackElo
whiteTitle   = Entry WhiteTitle
blackTitle   = Entry BlackTitle
whiteUSCF    = Entry WhiteUSCF
blackUSCF    = Entry BlackUSCF
whiteNA      = Entry WhiteNA
blackNA      = Entry BlackNA
whiteType    = Entry WhiteType
blackType    = Entry BlackType
eventDate    = Entry EventDate
eventSponsor = Entry EventSponsor
section      = Entry Section
stage        = Entry Stage
board        = Entry Board
opening      = Entry Opening
variation    = Entry Variation
subVariation = Entry SubVariation
eco          = Entry ECO
nic          = Entry NIC
time         = Entry Time
utcTime      = Entry UTCTime
utcDate      = Entry UTCDate
timeControl  = Entry TimeControl
setup        = Entry SetUp
fen          = Entry FEN
termination  = Entry Termination
plyCount     = Entry PlyCount
annotator    = Entry Annotator
mode         = Entry Mode
unknown      = Entry Unknown

rating  = Rated
address = Address

overTheBoard   = OTB
internetServer = ICS

createGame :: Entry Event 
           -> Entry Site  
           -> Entry Date
           -> Entry Round
           -> Entry White
           -> Entry Black
           -> Game
createGame event site date round white black =
    Game { entries = [HEntry event, 
                      HEntry site,
                      HEntry date,
                      HEntry round,
                      HEntry white,
                      HEntry black],
           gameBoard = Chess.emptyBoard }
