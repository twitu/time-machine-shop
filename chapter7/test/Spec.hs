import           Test.Tasty
import           Test.Tasty.HUnit
import           Lib
import           Data.List                      ( find )
import           Apriori
import           UnderAMonad
import           Data.Set                       ( fromList )
import           Graph
import           Control.Monad.Logic

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases =
  testGroup "Tests" [brokenJumpTest, findTest, clientToPurchaseInfoTest, pathsLTest, factorialTest]

brokenJumpTest = testCase "broken jumps should be the same" $ do
  (brokenJumps 3) @?= brokenThreeJumps

findTest = testCase "find' should work same as find" $ do
  find (> 4) [1, 3, 5, 3] @?= (find' (> 4) [1, 3, 5, 3])
  find (< 0) [1, 3, 5, 3] @?= (find' (< 0) [1, 3, 5, 3])

clientToPurchaseInfoTest = testCase "test purchase info generation" $ do
    (clientToPurchaseInfo
      (Company "1984 Inc." (Person "George" "Orwell" Male) "Director"))
    @?= (fromList
          [ InfoClientKind KindCompany
          , InfoClientDuty "Director"
          , InfoClientGender Male
          ]
        )

pathsLTest = testCase "test pathsL with do notation" $ do
    (observeMany 3 (pathsL graph2 2013 2558)) @?= (observeMany 3 (pathsL' graph2 2013 2558))

factorialTest = testCase "test factorial with monad implementation" $ do
    calcFactorial 4 @?= (product [1 .. 4])
