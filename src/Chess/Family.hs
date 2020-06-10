{-# LANGUAGE GADTs #-}

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

newtype Event = Event String
newtype Site = Site String
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

data Tag a where
  EventT        :: String     -> Tag Event
  SiteT         :: String     -> Tag Site
  RoundT        :: String     -> Tag Round
  DateT         :: String     -> Tag Date
  ResultT       :: Outcome    -> Tag Outcome
  WhiteEloT     :: Rating     -> Tag WhiteElo
  BlackEloT     :: Rating     -> Tag BlackElo
  WhiteTitleT   :: Title      -> Tag WhiteTitle
  BlackTitleT   :: Title      -> Tag BlackTitle
  WhiteUSCFT    :: String     -> Tag WhiteUSCF
  BlackUSCFT    :: String     -> Tag BlackUSCF
  WhiteNAT      :: Address    -> Tag WhiteNA
  BlackNAT      :: Address    -> Tag BlackNA
  WhiteTypeT    :: PlayerType -> Tag WhiteType
  BlackTypeT    :: PlayerType -> Tag BlackType
  EventDateT    :: String     -> Tag EventDate
  EventSponsorT :: String     -> Tag EventSponsor
  SectionT      :: String     -> Tag Section
  StageT        :: String     -> Tag Stage
  BoardT        :: String     -> Tag Board 
  OpeningT      :: String     -> Tag Opening
  VariationT    :: String     -> Tag Variation
  SubVariationT :: String     -> Tag SubVariation
  ECOT          :: String     -> Tag ECO 
  NICT          :: String     -> Tag NIC
  TimeT         :: String     -> Tag Time
  UTCTimeT      :: String     -> Tag UTCTime
  UTCDateT      :: String     -> Tag UTCDate
  TimeControlT  :: String     -> Tag TimeControl
  SetUpT        :: String     -> Tag SetUp
  FENT          :: String     -> Tag FEN
  TerminationT  :: Reason     -> Tag Termination
  PlyCountT     :: String     -> Tag PlyCount
  AnnotatorT    :: String     -> Tag Annotator
  ModeT         :: Variant    -> Tag Mode 
  UnknownT      :: String     -> String -> Tag a

event :: String -> Tag Event
event = EventT

site :: String -> Tag Site
site = SiteT

newgame :: Tag Event -> Tag Site -> String
newgame (EventT s) (SiteT x) = s <> " :: " <> x

extract :: [Tag a] -> Maybe a
extract [] = Nothing
extract ((EventT e) : _) = Just (Event e)