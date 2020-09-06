import           Test.Tasty
import           Test.Tasty.HUnit
import           KMeans
import           Lenses

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases = testGroup "Tests" [kMeansTest]

kMeansTest = testCase "Test KMeans" $ do
  assertEqual
    "simple initialize case"
    (let info = [(1, 1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
     in  kMeans initializeSimple 2 info 0.001
    )
    (3, [(4.0, 4.5), (1.0, 1.5)])
