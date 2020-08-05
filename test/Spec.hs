import Test.QuickCheck (quickCheck)
import Test.Hspec (hspec)
import ChessProps (gameConsistency)
import GameSpec (pgnFileSpec, lanSpec)

main :: IO ()
main = do
    -- quickCheck gameConsistency
    hspec lanSpec