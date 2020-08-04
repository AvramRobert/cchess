module GameSpec (pgnFileSpec, pgnGames) where

import qualified Chess as C
import qualified Chess.Internal as I
import qualified Parser.LAN as LP
import qualified Writer.LAN as LW
import Test.Hspec
import System.IO.Unsafe (unsafePerformIO)
import Data.List (all)
import Data.Either (isRight)

data Result = Game String deriving (Show)

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

-- for all games -> write LAN -> read LAN -> PGN read Move == LAN read Move
-- I could add short-circuiting
verifyLAN :: [C.Game] -> IO [C.Game]
verifyLAN = sequence . fmap verify
    where verify game     = fmap (const game) $ foldr isomorphy (return I.emptyBoard) $ I.past $ C.gameBoard game
          isomorphy m iob = iob >>= check m 
          check m b       = case (LP.parse b (LW.write m)) of
                            (Right _)  -> return $ I.forceApply b m
                            (Left _)   -> error ("Error while LAN-verifying move: " <> show m)

pgnFileSpec :: Spec
pgnFileSpec = do
    describe "PGN file games" $ do
        it "can be parsed and played" $ do
            computeGames pgnGames >> putStrLn "Successfully computed PGN games!"

lanSpec :: Spec
lanSpec = do
    describe "LAN games" $ do
        it "can be parsed, written and parsed again" $ do
            computeGames pgnGames >>= verifyLAN >> putStrLn "Successfully verified LAN games!"
