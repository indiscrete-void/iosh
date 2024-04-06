{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.ByteString qualified as B
import Data.Kind
import Data.List (singleton)
import Data.Maybe
import Data.Serialize
import GHC.Generics
import IOSH.Protocol
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.Serialize
import Test.Tasty
import Test.Tasty.HUnit

type TestMessage :: Type
data TestMessage where
  Data :: String -> TestMessage
  EOF :: TestMessage
  deriving stock (Eq, Show, Generic)

instance Serialize TestMessage

runTestIO :: [i] -> Sem (Input (Maybe i) : Output o : r) a -> Sem r [o]
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

testSerialization :: TestTree
testSerialization =
  testGroup
    "Serialization"
    [ testCase "run [serialize msg] deserialize == msg" $ testSerializeDeserializeIsId (singleton . serialize),
      testCase "run (split $ serialize msg) deserialzie == msg" $ testSerializeDeserializeIsId (split 3 . serialize),
      testCase "run (serializeOutput $ output msg) == serialize msg" $
        let runTest = listToMaybe . fst . run . runOutputList
         in runTest (serializeOutput @TestMessage $ output msg) @?= Just (serialize msg),
      testCase "run (deserializeInput $ input) == run deserialize" $
        let runTest = runM @IO . failToEmbed @IO . runDecoder . runInputList [serialize msg]
         in do
              (Just a) <- runTest (deserializeInput @TestMessage input)
              b <- runTest (deserialize @TestMessage)
              a @?= b
    ]
  where
    msg = EOF
    testSerializeDeserializeIsId ef =
      case run (runFail . runDecoder . runInputList (ef msg) $ deserialize) of
        Left e -> assertFailure e
        Right result -> result @?= msg
    split n str =
      if B.null str
        then []
        else
          let (part, rest) = B.splitAt n str
           in part : split n rest

tests :: TestTree
tests = testGroup "Unit Tests" [testSerialization, testTransferStream, testHandle]

main :: IO ()
main = defaultMain tests
