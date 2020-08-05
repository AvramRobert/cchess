module GameSpec (pgnFileSpec, lanSpec, pgnGames) where

import qualified Chess as C
import qualified Chess.Internal as I
import qualified Parser.LAN as LP
import qualified Writer.LAN as LW
import qualified Text.Megaparsec as M
import Test.Hspec
import System.IO.Unsafe (unsafePerformIO)
import Data.List (all, find)
import Data.Maybe (isJust)
import Data.Functor ((<&>))
import Control.Monad (join)


data SpecError = SpecError String deriving (Show)

finalise :: String -> Maybe SpecError -> IO ()
finalise msg (Nothing)          = putStrLn (msg <> " was successful!")
finalise msg (Just (SpecError e)) = error e

-- put this somewhere else
pgnGames :: [String]
pgnGames = unsafePerformIO $ C.pgnFromFile "./test/resources/games/carlsen.pgn" 

bind :: Monad f => (a -> f b) -> f a -> f b
bind f fa = fa >>= f

computeGames :: [String] -> IO [C.Game]
computeGames = sequence . fmap compute . zip [1..]
    where errorMsg error index game  = "Failed to compute PGN game " <> (show index) <> (show error) <> "\n" <> game
          compute (index, game)      = do
                    _ <- putStrLn ("Computing game " <> (show index))
                    case (C.parseGame game) of
                         (Right game) -> return game
                         (Left err)   -> error (errorMsg err index game) 
                         
verifyLAN :: [C.Game] -> IO [C.Game]
verifyLAN = sequence . fmap computeMoves
    where computeMoves game     = assert game $ foldl check id (I.past $ C.gameBoard game) (Right I.emptyBoard)
          assert game (Right _) = return game
          assert game (Left  e) = error e
          check f m (Right b) = case (LP.parse b (LW.write m)) of
              (Right m') | m == m' -> (Right b) `seq` f (Right $ I.forceApply b m')
              (Right m')           -> Left ("Parsed move: " <> show m' <> " is not equal to played move: " <> show m)
              (Left err)           -> Left ("Error while LAN-verifying move: " <> show m <> "\n\n" <> M.errorBundlePretty err)

pgnFileSpec :: Spec
pgnFileSpec = do
    describe "PGN file games" $ do
        it "can be parsed and played" $ do
            computeGames pgnGames >> putStrLn "Successfully computed PGN games!"

lanSpec :: Spec
lanSpec = do
    describe "LAN games" $ do
        it "can be parsed, written and parsed again" $ do
            computeGames pgnGames <&> verifyLAN >> putStrLn "Successfully computed LAN games!"