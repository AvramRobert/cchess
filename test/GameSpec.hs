module GameSpec (pgnFileSpec) where

import Test.Hspec
import qualified PGN as P
import System.IO.Unsafe (unsafePerformIO)
import Data.List (all)
import Data.Either (isRight)
import qualified PGN.Internal2 as PN

data Result = Game String deriving (Show)

pgnGames :: [String]
pgnGames = unsafePerformIO $ P.fromFile' "./test/resources/games/carlsen.pgn" 

computeGames :: [String] -> IO ()
computeGames = computeFrom 1 
    where computeFrom _ []     = putStrLn "Successfully finished!"
          computeFrom i (x:xs) =  do
            let computed = P.compute x
            _ <- putStrLn $ "Computing game " <> (show i)
            if (isRight computed)
            then computeFrom (i + 1) xs
            else error $ "Failed with game: " <> (show $ Game x)

pgnFileSpec :: Spec
pgnFileSpec = do
    describe "PGN file games" $ do
        it "can be parsed and played" $ do
            computeGames pgnGames