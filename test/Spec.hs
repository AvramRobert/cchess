import Test.QuickCheck
import ChessProps

main :: IO ()
main = quickCheck gameConsistency