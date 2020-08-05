module GameSpec (pgnFileSpec, lanSpec, pgnGames) where

import qualified Chess as C
import qualified Chess.Internal as I
import qualified Parser.LAN as LP
import qualified Writer.LAN as LW
import qualified Text.Megaparsec as M
import Test.Hspec
import System.IO.Unsafe (unsafePerformIO)
import Data.List (all)
import Data.Either (isRight)
import Data.Functor ((<&>))

data Result = Game String deriving (Show)

finalise :: String -> Either String a -> IO ()
finalise msg (Right _) = putStrLn (msg <> " was successful!")
finalise msg (Left  e) = error e

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

verifyLAN :: [C.Game] -> Either String [C.Game]
verifyLAN = sequence . fmap verify
    where verify game         = fmap (const game) $ ((foldl check id $ I.past $ C.gameBoard game) (Right I.emptyBoard))
          check f m (Right b) = case (LP.parseApply b (LW.write m)) of
              (Right nb)  -> (Right b) `seq` f (Right nb)
              (Left err)  -> Left ("Error while LAN-verifying move: " <> show m <> "\n\n" <> M.errorBundlePretty err)

pgnFileSpec :: Spec
pgnFileSpec = do
    describe "PGN file games" $ do
        it "can be parsed and played" $ do
            computeGames pgnGames >> putStrLn "Successfully computed PGN games!"

lanSpec :: Spec
lanSpec = do
    describe "LAN games" $ do
        it "can be parsed, written and parsed again" $ do
            computeGames [head pgnGames] <&> verifyLAN >>= (finalise "LAN games")