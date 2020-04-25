module ChessProps where

import qualified PGN as P
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Property (Property, forAll, succeeded, failed)
import System.IO.Unsafe (unsafePerformIO)

gameConsistency :: Property
gameConsistency = forAll games (successful . P.parse)
    where games = G.elements $ unsafePerformIO $ P.fromFile' "./test/resources/games/carlsen.pgn"
          successful (Right _) = succeeded
          successful (Left  _) = failed