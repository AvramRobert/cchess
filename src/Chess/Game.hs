{-# LANGUAGE GADTs #-}

module Chess.Game where

import qualified Chess.Internal as Chess

instance Eq HEntry where
  (==) (HEntry this) (HEntry that) = equate this that

instance Ord HEntry where
  compare (HEntry a) (HEntry b) = (rank a) `compare` (rank b)
    where rank (Entry EventTag _)  = 1
          rank (Entry SiteTag _)   = 2
          rank (Entry DateTag _)   = 3
          rank (Entry RoundTag _)  = 4
          rank (Entry WhiteTag _)  = 5
          rank (Entry BlackTag _)  = 6
          rank (Entry ResultTag _) = 7
          rank _                   = 8

equality :: Tag a -> a -> a -> Bool
equality EventTag = (==)

equate :: Entry a -> Entry b -> Bool
equate (Entry tag value) that = maybe False (equals value) (match tag that)
  where equals = equality tag

-- The `Show` for these things should be in Chess.Display
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

newtype Event = Event String deriving (Show, Eq)
newtype Site = Site String deriving (Show)
newtype Date = Date String deriving (Show)
newtype Round = Round String deriving (Show)
newtype White = White String deriving (Show)
newtype Black = Black String deriving (Show)
newtype Result = Result Outcome

newtype WhiteElo = WhiteElo Rating deriving (Show)
newtype BlackElo = BlackElo Rating deriving (Show)
newtype WhiteTitle = WhiteTitle Title deriving (Show)
newtype BlackTitle = BlackTitle Title deriving (Show)
newtype WhiteUSCF = WhiteUSCF String deriving (Show)
newtype BlackUSCF = BlackUSCF String deriving (Show)
newtype WhiteNA = WhiteNA Address deriving (Show)
newtype BlackNA = BlackNA Address deriving (Show)
newtype WhiteType = WhiteType PlayerType deriving (Show)
newtype BlackType = BlackType PlayerType deriving (Show)
newtype EventDate = EventDate String deriving (Show)
newtype EventSponsor = EventSponsor String deriving (Show)
newtype Section = Section String deriving (Show)
newtype Stage = Stage String deriving (Show)
newtype Board = Board String deriving (Show)

newtype Opening = Opening String deriving (Show)
newtype Variation = Variantion String deriving (Show)
newtype SubVariation = SubVariation String deriving (Show)

newtype ECO = ECO String deriving (Show)
newtype NIC = NIC String deriving (Show)
newtype Time = Time String deriving (Show)
newtype UTCTime = UTCTime String deriving (Show)
newtype UTCDate = UTCDate String deriving (Show)
newtype TimeControl = TimeControl String deriving (Show)

newtype SetUp = SetUp String deriving (Show)
newtype FEN = FEN String deriving (Show)

newtype Termination = Termination Reason deriving (Show)
newtype PlyCount = PlyCount String deriving (Show)
newtype Annotator = Annotator String deriving (Show)
newtype Mode = Mode Variant deriving (Show)
newtype Unknown = Unknown (String, String) deriving (Show)

data Tag a where
  EventTag        :: Tag Event
  SiteTag         :: Tag Site
  DateTag         :: Tag Date
  RoundTag        :: Tag Round
  WhiteTag        :: Tag White
  BlackTag        :: Tag Black
  ResultTag       :: Tag Result
  WhiteEloTag     :: Tag WhiteElo
  BlackEloTag     :: Tag BlackElo
  WhiteTitleTag   :: Tag WhiteTitle
  BlackTitleTag   :: Tag BlackTitle
  WhiteUSCFTag    :: Tag WhiteUSCF
  BlackUSCFTag    :: Tag BlackUSCF
  WhiteNATag      :: Tag WhiteNA
  BlackNATag      :: Tag BlackNA
  WhiteTypeTag    :: Tag WhiteType
  BlackTypeTag    :: Tag BlackType
  EventDateTag    :: Tag EventDate
  EventSponsorTag :: Tag EventSponsor
  SectionTag      :: Tag Section
  StageTag        :: Tag Stage
  BoardTag        :: Tag Board
  OpeningTag      :: Tag Opening
  VariationTag    :: Tag Variation
  SubVariationTag :: Tag SubVariation
  ECOTag          :: Tag ECO
  NICTag          :: Tag NIC
  TimeTag         :: Tag Time
  UTCTimeTag      :: Tag UTCTime
  UTCDateTag      :: Tag UTCDate
  TimeControlTag  :: Tag TimeControl
  SetUpTag        :: Tag SetUp
  FENTag          :: Tag FEN
  TerminationTag  :: Tag Termination
  PlyCountTag     :: Tag PlyCount
  AnnotatorTag    :: Tag Annotator
  ModeTag         :: Tag Mode
  UnknownTag      :: Tag Unknown

data Entry a where
  Entry :: Tag a -> a -> Entry a

data HEntry where
  HEntry :: Entry a -> HEntry

data Game = Game { entries :: [HEntry], 
                   board   :: Chess.Board }

match :: Tag a -> Entry b -> Maybe a
match EventTag (Entry EventTag v)               = Just v
match SiteTag  (Entry SiteTag v)                = Just v
match DateTag  (Entry DateTag v)                = Just v
match RoundTag (Entry RoundTag v)               = Just v
match WhiteTag (Entry WhiteTag v)               = Just v
match BlackTag (Entry BlackTag v)               = Just v
match ResultTag (Entry ResultTag v)             = Just v
match WhiteEloTag (Entry WhiteEloTag v)         = Just v
match BlackEloTag (Entry BlackEloTag v)         = Just v
match WhiteTitleTag (Entry WhiteTitleTag v)     = Just v
match BlackTitleTag (Entry BlackTitleTag v)     = Just v
match WhiteUSCFTag (Entry WhiteUSCFTag v)       = Just v
match BlackUSCFTag (Entry BlackUSCFTag v)       = Just v
match WhiteNATag (Entry WhiteNATag v)           = Just v
match BlackNATag (Entry BlackNATag v)           = Just v
match WhiteTypeTag (Entry WhiteTypeTag v)       = Just v
match BlackTypeTag (Entry BlackTypeTag v)       = Just v
match EventSponsorTag (Entry EventSponsorTag v) = Just v
match SectionTag (Entry SectionTag v)           = Just v
match StageTag (Entry StageTag v)               = Just v
match BoardTag (Entry BoardTag v)               = Just v
match OpeningTag (Entry OpeningTag v)           = Just v
match VariationTag (Entry VariationTag v)       = Just v
match SubVariationTag (Entry SubVariationTag v) = Just v
match ECOTag (Entry ECOTag v)                   = Just v
match NICTag (Entry NICTag v)                   = Just v
match TimeTag (Entry TimeTag v)                 = Just v
match UTCTimeTag (Entry UTCTimeTag v)           = Just v
match UTCDateTag (Entry UTCDateTag v)           = Just v
match TimeControlTag (Entry TimeControlTag v)   = Just v
match SetUpTag (Entry SetUpTag v)               = Just v
match FENTag (Entry FENTag v)                   = Just v
match TerminationTag (Entry TerminationTag v)   = Just v
match PlyCountTag (Entry PlyCountTag v)         = Just v
match AnnotatorTag (Entry AnnotatorTag v)       = Just v
match ModeTag (Entry ModeTag v)                 = Just v
match UnknownTag (Entry UnknownTag v)           = Just v
match _ _                                       = Nothing

determine :: Tag a -> [HEntry] -> Maybe a
determine _ []                    = Nothing
determine tag ((HEntry entry):es) = maybe (determine tag es) Just (match tag entry)

locate :: Tag a -> Game -> Maybe a
locate tag (Game entries board) = determine tag entries

add :: Entry a -> Game -> Game
add entry (Game entries board) = Game { entries = (HEntry entry) : entries, board = board }
 
newgame :: Entry Event -> Entry Site -> Game
newgame e s = Game { entries = [HEntry e, HEntry s], board = Chess.emptyBoard } 

event = Entry EventTag . Event
getEvent = locate EventTag
site = Entry SiteTag . Site