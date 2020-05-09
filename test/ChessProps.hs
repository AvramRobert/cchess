module ChessProps where

import qualified Chess as C
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Property (Property, forAll, succeeded, failed)
import System.IO.Unsafe (unsafePerformIO)

gameConsistency :: Property
gameConsistency = forAll games (successful . C.parseGame)
    where games = G.elements $ unsafePerformIO $ C.pgnFromFile "./test/resources/games/carlsen.pgn"
          successful (Right _) = succeeded
          successful (Left  _) = failed