module ChessProps where

import qualified PGN as P
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Property (Property, forAll, succeeded, failed)
import System.IO.Unsafe (unsafePerformIO)

gameConsistency :: Property
gameConsistency = forAll games (successful . P.computeGame)
    where games = G.elements $ unsafePerformIO $ P.fromPGN "./chess_games/batch0.pgn"
          successful (Right _) = succeeded
          successful (Left  _) = failed