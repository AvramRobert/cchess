module Parser.PGN where

import qualified Chess.Internal as Chess
import qualified Text.Megaparsec as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.List.NonEmpty as NL
import qualified Chess.Game as G
import Data.Monoid (Monoid)
import Data.ByteString.Lazy (ByteString)
import Text.Megaparsec (Parsec, (<|>), runParser, try, many)
import Text.Megaparsec.Char (char, char', string, string', spaceChar, numberChar, asciiChar, newline)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Control.Monad (void, forM)
import Data.Functor (($>), (<&>))
import Data.List (find, sort)
import System.IO.Unsafe (unsafePerformIO)
import Lib.Coll
import Lib.Megaparsec
import Chess.Display
import Parser.Common

data PGNError = CaptureError Chess.Coord Chess.Figure |
                AdvanceError Chess.Coord Chess.Figure |
                PromoteError Chess.Coord Chess.Figure | 
                CastleError  Chess.Castles            | 
                MissingMovesError                     |
                UnexpectedCheckError
                deriving (Eq, Show, Ord)

instance M.ShowErrorComponent PGNError where
    showErrorComponent (CaptureError coord piece) = "Could not capture at " <> (show coord) <> " with " <> (show piece)
    showErrorComponent (AdvanceError coord piece) = "Could not advance to " <> (show coord) <> " with " <> (show piece)
    showErrorComponent (PromoteError coord piece) = "Could not promote at " <> (show coord) <> " to "   <> (show piece)
    showErrorComponent (CastleError c)            = "Could not castle: " <> (show c) 
    showErrorComponent (MissingMovesError)        = "No moves available"
    showErrorComponent (UnexpectedCheckError)     = "Board was not expected to be in check"

type Parser a = Parsec PGNError String a

type PGNParseError = ParseError PGNError

data Turn = Ended | Moved deriving (Show, Eq)

extractGame :: [ByteString] -> (String, [ByteString])
extractGame lines = (C.unpack $ C.unlines (header <> gameLines), remaining)
        where header    = takeWhile headline lines
              headless  = dropWhile headline lines
              gameLines = takeWhile (not . headline) headless
              remaining = dropWhile (not . headline) headless
              headline  = every [not . C.null, (== '[') . C.head]

result :: Parser G.Outcome
result = M.choice [try (string "1-0")     $> G.WhiteWin,
                   try (string "0-1")     $> G.BlackWin,
                   try (string "1/2-1/2") $> G.Draw,
                   try (string "*")       $> G.Other]

rating :: Parser G.Rating
rating = M.choice [try (string "-") $> G.Unrated,
                   try (characters) <&> G.Rated]

title :: Parser G.Title
title = M.choice [try (string' "GM") $> G.GM,
                  try (string' "FM") $> G.FM,
                  try (string' "IM") $> G.IM,
                  try (string' "-")  $> G.UT]

address :: Parser G.Address
address = M.choice [try (string "-") $>  G.NoAddress,
                    try (characters) <&> G.Address]

playerType :: Parser G.PlayerType
playerType = M.choice [try (string' "human")    $> G.Human,
                       try (string' "computer") $> G.Computer]

reason :: Parser G.Reason
reason = M.choice [try (string' "abandoned")        $> G.Abandoned,
                   try (string' "adjudication")     $> G.Adjundication,
                   try (string' "death")            $> G.Death,
                   try (string' "emergency")        $> G.Emergency,
                   try (string' "normal")           $> G.Normal,
                   try (string' "rules infraction") $> G.Infraction,
                   try (string' "time forfeit")     $> G.TimeForfeit,
                   try (string' "Unterminated")     $> G.Unterminated]

variant :: Parser G.GameMode
variant = M.choice [try (string' "otb") $> G.OTB,
                    try (string' "ics") $> G.ICS]

tagline :: Parser a -> Parser b -> Parser (a, b)
tagline title content = do
        _ <- delimitation
        _ <- char '['
        t <- title
        _ <- delimitation
        a <- M.between (char '"') (char '"') content
        _ <- char ']'
        _ <- delimitation
        return (t, a)

entryParser :: [Parser G.HEntry]
entryParser = [try $ extract "Event" characters G.event, 
               try $ extract "Site" characters G.site, 
               try $ extract "Date" characters G.date,
               try $ extract "Round" characters G.round,
               try $ extract "White" characters G.white,
               try $ extract "Black" characters G.black,
               try $ extract "Result" result G.result,
               try $ extract "WhiteElo" rating G.whiteElo,
               try $ extract "Blackelo" rating G.blackElo,
               try $ extract "WhiteTitle" title G.whiteTitle,
               try $ extract "BlackTitle" title G.blackTitle,
               try $ extract "WhiteUSCF" characters G.whiteUSCF,
               try $ extract "BlackUSCF" characters G.blackUSCF,
               try $ extract "WhiteNA" address G.whiteNA,
               try $ extract "BlackNA" address G.blackNA,
               try $ extract "WhiteType" playerType G.whiteType,
               try $ extract "BlackType" playerType G.blackType,
               try $ extract "EventDate" characters G.eventDate,
               try $ extract "EventSponsor" characters G.eventSponsor,
               try $ extract "Section" characters G.section,
               try $ extract "Stage" characters G.stage,
               try $ extract "Board" characters G.board,
               try $ extract "Opening" characters G.opening,
               try $ extract "Variation" characters G.variation,
               try $ extract "SubVariation" characters G.subVariation,
               try $ extract "ECO" characters G.eco,
               try $ extract "NIC" characters G.nic,
               try $ extract "Time" characters G.time,
               try $ extract "UTCTime" characters G.utcTime,
               try $ extract "UTCDate" characters G.utcDate,
               try $ extract "TimeControl" characters G.timeControl,
               try $ extract "Setup" characters G.setup,
               try $ extract "FEN" characters G.fen,
               try $ extract "Termination" reason G.termination,
               try $ extract "PlyCount" characters G.plyCount,
               try $ extract "Annotator" characters G.annotator,
               try $ extract "Mode" variant G.mode,
               unknown]
    where extract title what as = tagline (string' title) what <&> (G.HEntry . as . snd)
          unknown               = tagline characters characters <&> (G.HEntry . G.unknown)

-- Some PGN files exports don't abide to the format rules..
-- I have to parse without ordering and the order them properly later on
entriesParser :: Parser [G.HEntry]
entriesParser = (M.optional (M.choice entryParser) >>= process)
    where process (Just h)  = fmap (\hs -> h:hs) entriesParser
          process (Nothing) = return []
          
headline :: String -> Parser a -> Parser a
headline header p = do
        _ <- delimitation
        _ <- char '['
        _ <- string header
        _ <- delimitation
        a <- M.between (char '"') (char '"') p
        _ <- char ']'
        _ <- delimitation
        return a

check :: Chess.Board -> Parser ()
check board = (M.optional $ M.single '+') >>= verify
    where verify (Just _) | Chess.check board = return ()
          verify (Nothing)                    = return ()
          verify (Just _)                     = failWith UnexpectedCheckError Nothing

-- the [Chess.Move] list is here just so that I can extract figure information for it for the log
-- I could replace it with the `Figure` itself to make it more explicit
captureError :: Chess.Coord -> [Chess.Move] -> PGNError
captureError _ []    = MissingMovesError
captureError c (m:_) = CaptureError c (Chess.figure $ Chess.position m)

promoteError :: Chess.Position -> [Chess.Move] -> PGNError
promoteError _ []    = MissingMovesError
promoteError p _     = PromoteError (Chess.coord p) (Chess.figure p)

advanceError :: Chess.Coord -> [Chess.Move] -> PGNError
advanceError _ []    = MissingMovesError
advanceError c (m:_) = AdvanceError c $ (Chess.figure $ Chess.position m)

enpassantCapture :: [Chess.Move] -> Parser Chess.Move
enpassantCapture moves = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [enpassantAt (x, y), hasX ox]) moves

-- there's only one piece I can capture at `x, y`
unambigousCapture :: [Chess.Move] -> Parser Chess.Move
unambigousCapture moves = do 
    _ <- char 'x'
    x <- file
    y <- rank
    failWith (captureError (x, y) moves) $ find (capturesAt (x, y)) moves

-- there's a piece which sits at file `ox` and can capture `x, y`
fileAmbigousCapture :: [Chess.Move] -> Parser Chess.Move
fileAmbigousCapture moves = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [capturesAt (x, y), hasX ox]) moves 

-- there's a piece which sits at rank `oy` and can capture `x, y`
rankAmbigousCapture :: [Chess.Move] -> Parser Chess.Move
rankAmbigousCapture moves = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [capturesAt (x, y), hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can capture `x, y`
explicitCapture :: [Chess.Move] -> Parser Chess.Move
explicitCapture moves = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [capturesAt (x, y), hasCoord (ox, oy)]) moves

-- there's only one piece which can advance to square `x, y`
unambigousAdvance :: [Chess.Move] -> Parser Chess.Move
unambigousAdvance moves = do 
    x <- file
    y <- rank
    failWith (advanceError (x, y) moves) $ find (advancesTo (x, y)) moves 

-- there's a piece which sits at file `ox` and can advance to `x, y`
fileAmbigousAdvance :: [Chess.Move] -> Parser Chess.Move
fileAmbigousAdvance moves = do
    ox <- file
    x  <- file
    y  <- rank
    failWith (advanceError (x, y) moves) $ find (every [advancesTo (x, y), hasX ox]) moves

-- there's a piece which sits at rank `oy` and can advance to `x, y`
rankAmbigousAdvance :: [Chess.Move] -> Parser Chess.Move
rankAmbigousAdvance moves = do
    oy <- rank
    x  <- file
    y  <- rank
    failWith (advanceError (x, y) moves) $ find (every [advancesTo (x, y), hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can advance to `x, y`
explicitAdvance :: [Chess.Move] -> Parser Chess.Move
explicitAdvance moves = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    failWith (advanceError (x, y) moves) $ find (every [advancesTo (x, y), hasCoord (ox, oy)]) moves

advance :: [Chess.Move] -> Parser Chess.Move
advance moves = M.choice [try $ unambigousAdvance moves,
                          try $ fileAmbigousAdvance moves,
                          try $ rankAmbigousAdvance moves,
                          try $ explicitAdvance moves] 

capture :: [Chess.Move] ->  Parser Chess.Move
capture moves = M.choice [try $ unambigousCapture moves,
                          try $ fileAmbigousCapture moves,
                          try $ rankAmbigousCapture moves,
                          try $ explicitCapture moves]

captureOrAdvance :: [Chess.Move] -> Parser Chess.Move
captureOrAdvance moves = M.choice [capture moves, advance moves] 

-- there's a pawn at file `ox` that can capture `x, y` enapssant
enpassantPawn :: [Chess.Move] -> Parser Chess.Move
enpassantPawn = enpassantCapture

-- there's a pawn at file `ox` that can capture `x, y`
capturePawn :: [Chess.Move] -> Parser Chess.Move
capturePawn = fileAmbigousCapture

-- there's a pawn at file `ox` that can advnace to `x, y`
advancePawn :: [Chess.Move] -> Parser Chess.Move
advancePawn moves = M.choice [try $ unambigousAdvance moves, try $ explicitAdvance moves] 

-- there's a pawn at promoting position `x, y -+ 1` and can promote at `x, y`
promotePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
promotePawn colour moves = do
        x <- file
        y <- rank
        _ <- char '='
        p <- promotions (colour, (x, y))
        let from = pick colour (x, y)
        failWith (promoteError p moves) $ find (promotesAs p from) moves
    where pick Chess.W (x, y) = (x, y - 1)
          pick Chess.B (x, y) = (x, y + 1) 

-- there's a pawn at a promoting position `ox, y +- 1` and can take left and promote at `x, y`
capturePromotePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
capturePromotePawn colour moves = do
        ox <- file
        _  <- char 'x'
        x  <- file
        y  <- rank
        _  <- char '='
        p  <- promotions (colour, (x, y))
        let from = pick colour (ox, y)
        failWith (promoteError p moves) $ find (promotesAs p from) moves
    where pick Chess.W (x, y) = (x, y - 1)
          pick Chess.B (x, y) = (x, y + 1)

castle :: Chess.Castles -> [Chess.Move] -> Parser Chess.Move
castle Chess.Long  = failWith (CastleError Chess.Long) . find (castlesTowards Chess.L)
castle Chess.Short = failWith (CastleError Chess.Short) . find (castlesTowards Chess.R)
castle _           = const $ failWith MissingMovesError Nothing  

-- there's an ordering problem here aswell
-- An unambigous advance may be eagerly interpreted as a promotion in certain scenarios
-- Ex: d1=Q => `d1` by itself is a valid unambigous advance.
-- We thus have to start with checking promotions and then the rest..

-- SHIT: The pawn parser doesn't see enpassants. FUCK
pawn :: Chess.Board -> Parser Chess.Move
pawn board = M.choice [try $ promotePawn colour moves,
                       try $ capturePromotePawn colour moves,
                       try $ capturePawn moves,
                       try $ enpassantPawn moves,
                       try $ advancePawn moves]
        where moves  = Chess.movesFigure board (Chess.Pawn, colour)
              colour = Chess.player board 
                

rook :: Chess.Board -> Parser Chess.Move
rook board = char' 'R' >> (captureOrAdvance $ Chess.movesFigure board (Chess.Rook, Chess.player board))

bishop :: Chess.Board -> Parser Chess.Move
bishop board = char' 'B' >> (captureOrAdvance $ Chess.movesFigure board (Chess.Bishop, Chess.player board))

knight :: Chess.Board -> Parser Chess.Move
knight board = char' 'N' >> (captureOrAdvance $ Chess.movesFigure board (Chess.Knight, Chess.player board))

queen :: Chess.Board -> Parser Chess.Move
queen board = char' 'Q' >> (captureOrAdvance $ Chess.movesFigure board (Chess.Queen, Chess.player board))

-- there's an ordering problem here due to `string`
-- if `O-O` comes before `O-O-O`, `string` will catch a long castle with it
king :: Chess.Board -> Parser Chess.Move
king board = M.choice [try $ char' 'K'       >> (captureOrAdvance moves),
                       try $ string' "O-O-O" >> (castle Chess.Long moves),
                       try $ string' "O-O"   >> (castle Chess.Short moves)]
    where moves  = Chess.movesFigure board (Chess.King, Chess.player board)

move :: Chess.Board -> Parser Chess.Move
move board = M.choice [try $ pawn board,
                       try $ rook board,
                       try $ bishop board,
                       try $ knight board,
                       try $ queen board,
                       try $ king board]

moveParser :: Chess.Board -> Parser Chess.Move
moveParser board = do
    _ <- delimitation
    m <- move board
    _ <- delimitation
    return m

appliedMoveParser :: Chess.Board -> Parser (Chess.Board, Chess.Move)
appliedMoveParser board = do
    m  <- moveParser board
    let b' = Chess.forceApply board m
    _  <- check b'
    _  <- mate
    _  <- delimitation
    return (b', m)

appliedTurnParser :: Chess.Board -> Parser (Chess.Board, Turn)
appliedTurnParser board = do
    (b', _) <- appliedMoveParser board
    o       <- M.optional $ M.lookAhead result
    return $ case o of (Just _)     -> (b', Ended)
                       (Nothing)    -> (b', Moved)

turnParser :: Chess.Board -> Parser (Chess.Board, Turn)
turnParser board = (try firstTurn) <|> (try secondTurn)
    where firstTurn  = delimitation >> index >> appliedTurnParser board
          secondTurn = delimitation >> appliedTurnParser board  

boardParser :: Parser (Chess.Board, Turn)
boardParser = parseOn Chess.emptyBoard
    where parseOn board = turnParser board >>= continue
          continue (b, Moved) = parseOn b
          continue (b, t)     = return (b, t)

gameParser :: Parser G.Game
gameParser = do
    _             <- delimitation
    entries       <- entriesParser
    _             <- delimitation
    (board, turn) <- boardParser
    result        <- result
    return G.Game { G.entries = entries, G.gameBoard = board }

splitGames :: ByteString -> [String]
splitGames = accumulate . C.lines
    where accumulate [] = []
          accumulate ls = let (game, remaining) = extractGame ls
                          in game : (accumulate remaining)

fromPGNFile' :: String -> IO [String]
fromPGNFile' = fmap splitGames . B.readFile

fromString' :: String -> [String]
fromString' = splitGames . C.pack

fromPGNFile :: String -> IO (Either PGNParseError [G.Game])
fromPGNFile = fmap (sequence . fmap (run gameParser)) . fromPGNFile'

fromString :: String -> Either PGNParseError [G.Game]
fromString = sequence . fmap (run gameParser) . splitGames . C.pack

parseGame :: String -> Either PGNParseError G.Game
parseGame = run gameParser

parseMove ::  String -> Chess.Board -> Either PGNParseError Chess.Move
parseMove move board = run (moveParser board) move

parseBoard :: String -> Either PGNParseError Chess.Board
parseBoard = fmap G.gameBoard . parseGame