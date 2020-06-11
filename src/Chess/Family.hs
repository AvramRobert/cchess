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
newtype Unknown = Unknown (String, String)

data DynTag where
  DynTag :: Tag a -> DynTag 

data Tag a where
  TEvent :: String -> Tag Event
  TSite  :: String -> Tag Site
  TDate :: String  -> Tag Date
  TRound :: String -> Tag Round
  TWhite :: String -> Tag White
  TBlack :: String -> Tag Black
  TResult :: Outcome -> Tag Result
  TWhiteElo :: Rating -> Tag WhiteElo
  TBlackElo :: Rating -> Tag BlackElo
  TWhiteTitle :: Title -> Tag WhiteTitle
  TBlackTitle :: Title -> Tag BlackTitle
  TWhiteUSCF :: String -> Tag WhiteUSCF
  TBlackUSCF :: String -> Tag BlackUSCF
  TWhiteNA :: Address -> Tag WhiteNA
  TBlackNA :: Address -> Tag BlackNA
  TEventDate :: String -> Tag EventDate
  TEventSponsor :: String -> Tag EventSponsor
  TSection :: String -> Tag Section
  TStage :: String -> Tag Stage
  TBoard :: String -> Tag Board
  TOpening :: String -> Tag Opening
  TVariation :: String -> Tag Variation
  TSubVariation :: String -> Tag SubVariation
  TECO :: String -> Tag ECO
  TNIC :: String -> Tag NIC
  TTime :: String -> Tag Time
  TUTCTime :: String -> Tag UTCTime
  TUTCDate :: String -> Tag UTCDate
  TTimeControl :: String -> Tag TimeControl
  TSetUp :: String -> Tag SetUp
  TFEN :: String -> Tag FEN
  TTermination :: Reason -> Tag Termination
  TPlyCount :: String -> Tag PlyCount
  TAnnotator :: String -> Tag Annotator
  TMode :: Variant -> Tag Mode
  TUnknown :: String -> String -> Tag Unknown

event :: String -> Tag Event
event = TEvent

data Game = Game { tags :: [DynTag], board :: Chess.Board }

recurseFind :: (forall a . Tag a -> Maybe b) -> [DynTag] -> Maybe b
recurseFind f []                = Nothing
recurseFind f ((DynTag t) : ts) = maybe (recurseFind f ts) Just $ f t 

add :: Tag a -> Game -> Game
add t game = game { tags = (DynTag t) : (tags game) }

findAny :: (forall a . Tag a -> Maybe b) -> Game -> Maybe b
findAny f (Game tags board) = recurseFind f tags 

event' :: Tag a -> Maybe Event
event' (TEvent s) = Just $ Event s
event' _          = Nothing

getEvent :: Game -> Maybe Event
getEvent = findAny event'
