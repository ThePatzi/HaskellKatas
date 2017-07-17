import           Test.HUnit
import           Test.QuickCheck

yourOrderPlease :: String -> String
yourOrderPlease str = unwords $ words str

main :: IO()
main = runTests

tests :: Test
tests = TestList $ map TestCase
  [
    assertEqual "First Test" "Thi1s is2 3a T4est" (yourOrderPlease "is2 Thi1s T4est 3a")
  ]

prop_empty :: Int -> Bool
prop_empty c1 = (c1 :: Int) == c1

runTests :: IO()
runTests = do
  _ <- runTestTT tests
  quickCheck prop_empty
