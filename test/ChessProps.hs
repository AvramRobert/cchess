module ChessProps where

import Chess
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Property (Property, forAll, (===))
import qualified Test.QuickCheck.Gen as G
import Control.Monad.Zip (mzip, MonadZip)

instance MonadZip Gen where
    mzip gena genb = pure (\a b -> (a, b)) <*> gena <*> genb

blackBoard :: Gen Board
blackBoard = fmap switch $ pure board
    where switch board | B == player board = board
          switch board = changeTurn board

whiteBoard :: Gen Board
whiteBoard = fmap changeTurn $ blackBoard

whiteMove :: Gen Move
whiteMove = pure $ Promote (Pawn W (1, 3)) (Queen W (1, 3))

blackMove :: Gen Move
blackMove = pure $ Promote (Pawn B (1, 3)) (Queen B (1, 3))

turnConsistency :: Property
turnConsistency = forAll (G.oneof [blacks, whites]) cannotMove
    where cannotMove (m, b) = (=== Illegal) $ fst $ (move m b)
          blacks = mzip blackMove whiteBoard
          whites = mzip whiteMove blackBoard
 
-- instead of thinking of how to arbitrarily generate correct chess boards,
-- what I could do is i could download a bunch of chess games and write a parser for their serialised form
-- parse each one of the games into a series of moves and then go through each move, checking if my implementation allows them
-- If they're allowed, it's good, otherwise I might've been missing something.