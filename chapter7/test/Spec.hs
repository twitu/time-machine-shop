import           Test.Tasty
import           Test.Tasty.HUnit
import           Lib

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases = testGroup "Tests" [brokenJumpTest]

brokenJumpTest = testCase "broken jumps should be the same" $ do
  (brokenJumps 3) @?= brokenThreeJumps
