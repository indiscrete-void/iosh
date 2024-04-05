{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Kind
import Data.Serialize
import GHC.Generics
import IOSH.Protocol
import Polysemy qualified as Sem
import Polysemy.Input
import Polysemy.Output
import Test.Tasty
import Test.Tasty.HUnit

type TestMessage :: Type
data TestMessage where
  Data :: String -> TestMessage
  EOF :: TestMessage
  deriving stock (Eq, Show, Generic)

instance Serialize TestMessage

testTransferStream :: TestTree
testTransferStream =
  testGroup
    "transferStream"
    [ testCase "Outputs all input values wrapped in passed f and passed message after EOF" $
        let inputList = ["a", "b", "c"]
            outputList = map Data inputList ++ [EOF]
            run = fst . Sem.run . runOutputList . runInputList inputList
         in run (transferStream Data EOF) @?= outputList
    ]

tests :: TestTree
tests = testGroup "Unit Tests" [testTransferStream]

main :: IO ()
main = defaultMain tests
