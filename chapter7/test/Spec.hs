import           Test.Tasty
import           Test.Tasty.HUnit
import           Lib
import           Data.List                      ( find )
import           Apriori
import           Data.Set                       ( fromList )

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases =
  testGroup "Tests" [brokenJumpTest, findTest, clientToPurchaseInfoTest]

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
