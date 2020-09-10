import           Test.Tasty
import           Test.Tasty.HUnit
import           Lib
import           Data.List                      ( find )

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases = testGroup "Tests" [brokenJumpTest, findTest]

brokenJumpTest = testCase "broken jumps should be the same" $ do
  (brokenJumps 3) @?= brokenThreeJumps

findTest = testCase "find' should work same as find" $ do
  find (> 4) [1, 3, 5, 3] @?= (find' (> 4) [1, 3, 5, 3])
  find (< 0) [1, 3, 5, 3] @?= (find' (< 0) [1, 3, 5, 3])
