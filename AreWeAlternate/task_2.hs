import           Data.List
import           Test.HUnit
import           Test.QuickCheck

consonants :: String
consonants = ['a', 'e', 'i', 'o', 'u']

isAlt :: String -> Bool
isAlt = all ((==1) . length) .group . map (\ v -> if v `elem` consonants then 1 else 0)

main :: IO()
main = runTests

tests :: Test
tests = TestList $ map TestCase
  [
    assertEqual "Run isAlt with 'test'" False (isAlt "test"),
    assertEqual "Run isAlt with 'banana'" True (isAlt "banana"),
    assertEqual "Run isAlt with 'apple'" False (isAlt "apple"),
    assertEqual "Run isAlt with 'amazon'" True (isAlt "amazon"),
    assertEqual "Run isAlt with ''" True (isAlt "")
  ]

prop_empty :: Int -> Bool
prop_empty c1 = (c1 :: Int) == c1

runTests :: IO()
runTests = do
  runTestTT tests
  quickCheck prop_empty
