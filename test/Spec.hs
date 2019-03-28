import Test.QuickCheck

idProp :: Eq a => a -> Bool
idProp a = a == a


main :: IO ()
main = quickCheck (idProp :: Int -> Bool)
