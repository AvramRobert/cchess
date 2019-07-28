import Test.QuickCheck (quickCheck)
import Test.Hspec (hspec)
import ChessProps (gameConsistency)
import GameSpec (pgnFileSpec)

main :: IO ()
main = do
    -- quickCheck gameConsistency
    hspec pgnFileSpec