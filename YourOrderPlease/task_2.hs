import           Data.Char
import           Data.List
import           Data.Maybe
import           Test.HUnit
import           Test.QuickCheck

yourOrderPlease :: String -> String
yourOrderPlease str = unwords $ sortOn (find isDigit) (words str)

main :: IO()
main = runTests

tests :: Test
tests = TestList $ map TestCase
  [
    assertEqual "First Test" "Thi1s is2 3a T4est" (yourOrderPlease "is2 Thi1s T4est 3a"),
    assertEqual "Second test" "Fo1r the2 g3ood 4of th5e pe6ople" (yourOrderPlease "4of Fo1r pe6ople g3ood th5e the2")
  ]

prop_empty :: Int -> Bool
prop_empty c1 = (c1 :: Int) == c1

runTests :: IO()
runTests = do
  _ <- runTestTT tests
  quickCheck prop_empty
