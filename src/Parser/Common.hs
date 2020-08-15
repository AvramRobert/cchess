module Parser.Common where

import qualified Chess.Internal as Chess
import Data.Set (fromList)
import Text.Megaparsec (ErrorFancy (ErrorCustom), ParseErrorBundle, Parsec, 
                        fancyFailure, try, many, manyTill, someTill, choice,
                        lookAhead, runParser, single, optional)
import Text.Megaparsec.Char (char, char', asciiChar, numberChar, spaceChar, newline)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (digitToInt)

type ParseError e = ParseErrorBundle String e

failWith :: Ord e => e -> Maybe a -> Parsec e String a
failWith error (Nothing) = fancyFailure $ fromList [ErrorCustom error]
failWith error (Just a)  = return a

-- apparently there's no sane way to consume any arbitrary ascii characters between two quotes, without forcing the user to lookAhead for the quote
-- between (char '"') (char '"') (many asciiChar) will fail because (many asciiChar) will consume the last '"' thus `between` will never work
-- so it has to be this: between (char '"') (char '"') (manyTill asciiChar (lookAhead (char '"')))
characters :: Ord e => Parsec e String String
characters = manyTill asciiChar (lookAhead $ char '"')

delimitation :: Ord e => Parsec e String ()
delimitation = void $ many (try newline <|> try spaceChar)

index :: Ord e => Parsec e String String
index = someTill numberChar (char '.')

file :: Ord e => Parsec e String Int
file = choice [(char 'a' $> 1),
               (char 'b' $> 2), 
               (char 'c' $> 3),
               (char 'd' $> 4),
               (char 'e' $> 5),
               (char 'f' $> 6),
               (char 'g' $> 7),
               (char 'h' $> 8)]

rank :: Ord e => Parsec e String Int
rank = fmap digitToInt $ numberChar

piece :: Ord e => Parsec e String Chess.Piece
piece = choice [try (char' 'Q' $> Chess.Queen),
                try (char' 'R' $> Chess.Rook),
                try (char' 'N' $> Chess.Knight),
                try (char' 'B' $> Chess.Bishop)]

mate :: Ord e => Parsec e String Bool
mate = fmap (maybe False (const True)) $ optional $ single '#'

promotions :: Ord e => Chess.Square -> Parsec e String Chess.Position
promotions (c, s) = fmap (\p -> Chess.Pos p c s) piece

hasColour :: Chess.Colour -> Chess.Move -> Bool
hasColour colour = (== colour) . Chess.colour . Chess.position

hasCoord :: Chess.Coord -> Chess.Move -> Bool
hasCoord coord = (== coord) . Chess.coord . Chess.position

hasPiece :: Chess.Piece -> Chess.Move -> Bool
hasPiece piece = (== piece) . Chess.piece . Chess.position

hasX :: Int -> Chess.Move -> Bool
hasX x = (== x) . fst . Chess.coord . Chess.position

hasY :: Int -> Chess.Move -> Bool
hasY y = (== y) . snd . Chess.coord . Chess.position

advancesTo :: Chess.Coord -> Chess.Move -> Bool
advancesTo s (Chess.Advance _ e) = s == e
advancesTo _ _                    = False

capturesAt :: Chess.Coord -> Chess.Move -> Bool
capturesAt s (Chess.Capture _ enemy) = s == (Chess.coord enemy)
capturesAt _ _                       = False

enpassantAt :: Chess.Coord -> Chess.Move -> Bool
enpassantAt s (Chess.Enpassant _ e _) = s == e
enpassantAt _ _                       = False

promotesAs :: Chess.Position -> Chess.Coord -> Chess.Move -> Bool
promotesAs newPos from (Chess.Promote pawn newPiece enemy) = (Chess.coord pawn) == from && 
                                                             (Chess.piece newPos) == newPiece && 
                                                             (Chess.coord newPos) == (Chess.coord enemy)
promotesAs _ _ _                                           = False

castlesTowards :: Chess.Dir -> Chess.Move -> Bool
castlesTowards Chess.R (Chess.Castle (_, e) _) = e == (7, 1) || e == (7, 8)
castlesTowards Chess.L (Chess.Castle (_, e) _) = e == (3, 1) || e == (3, 8)
castlesTowards _ _                             = False