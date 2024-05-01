{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Kind
import GHC.Generics
import IOSH.Protocol
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Test.Tasty
import Test.Tasty.HUnit

type TestMessage :: Type
data TestMessage where
  Data :: String -> TestMessage
  EOF :: TestMessage
  deriving stock (Eq, Show, Generic)

runTestIO :: [i] -> Sem (Input (Maybe i) ': Output o ': r) a -> Sem r [o]
runTestIO inputList sem = fst <$> runOutputList (runInputList inputList sem)

testTransferStream :: TestTree
testTransferStream =
  testGroup
    "transferStream"
    [ testCase "Outputs all input values wrapped in passed f and passed message after EOF" $
        let inputList = ["a", "b", "c"]
            outputList = map Data inputList ++ [EOF]
         in run (runTestIO inputList $ transferStream Data EOF) @?= outputList
    ]

testHandle :: TestTree
testHandle =
  testGroup
    "handle"
    [ testCase "Calls handler function with received message" $
        let messageList = [Data "test", EOF]
         in run (runTestIO messageList $ handle @TestMessage output) @?= messageList
    ]

tests :: TestTree
tests = testGroup "Unit Tests" [testTransferStream, testHandle]

main :: IO ()
main = defaultMain tests
