import           Test.Tasty
import           Test.Tasty.HUnit
import           KMeansLenses
import           Lenses
import           MyWriter

main :: IO ()
main = defaultMain testCases

testCases :: TestTree
testCases = testGroup "Tests" [kMeansLensesTest]
-- testCases = testGroup "Tests" [kMeansLensesTest, myWriterTest]

kMeansLensesTest = testCase "Test KMeans" $ do
  assertEqual
    "simple initialize case"
    (let info = [(1, 1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
     in  kMeans initializeSimple 2 info 0.001
    )
    (3, [(4.0, 4.5), (1.0, 1.5)])

-- myWriterTest = testCase "Test MyWriter monad" $ do
--   assertEqual "simple computation case"
--   runWriter
--     (do
--       tell "Start"
--       return (1 + 1)
--       tell "Finish"
--     )
--   "StartFinish"
