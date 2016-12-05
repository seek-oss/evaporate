module LoggingSpec (spec, main) where

import Control.Monad.Except (runExceptT)
import Data.Either.Combinators (fromRight')
import Data.Monoid ((<>))
import Test.Hspec ( describe
                  , context
                  , shouldBe
                  , shouldSatisfy
                  , it
                  , hspec
                  , Spec
                  )

import Configuration (Command(..))
import Logging ( logMain
               , logGeneral
               , logStackName
               , logExecution
               , logStackOutputs
               , filterBuilderBy
               , LogParameters(..)
               )
import StackParameters (getStackParameters, StackDescription(..))
import Types ( StackName(..)
             , StackOutputName(..)
             )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "LoggingSpec" $ do
  context "generating log messages" $ do
    it "can generate a main log message" $ do
      stackParams <- runExceptT . getStackParameters $ "test/valid.yaml"
      let descriptions = fromRight' stackParams
      let accountID = "478156153062"
      let logParams = LogParameters Create descriptions accountID
      let logMessage = logMain logParams
      logMessage `shouldBe`
           "\nCommand being executed: "
        <> "Create"
        <> "\nAWS Account ID: "
        <> "478156153062"
        <> "\nStack(s) being operated on:"
        <> "\n    Stack1"
        <> "\n    Stack2"

    it "can generate a general log message" $ do
      let accountID = "478156153062"
      let logMessage = logGeneral Create accountID
      logMessage `shouldBe`
           "\nCommand being executed: "
        <> "Create"
        <> "\nAWS Account ID: "
        <> "478156153062"
        <> "\nStack(s) being operated on:"

    it "can generate a stack name log message" $ do
      stackParams <- runExceptT . getStackParameters $ "test/valid.yaml"
      let desc = head . fromRight' $ stackParams
      let logMessage = logStackName desc
      logMessage `shouldBe` "\n    Stack1"

    it "can generate a command execution log message" $ do
      stackParams <- runExceptT . getStackParameters $ "test/valid.yaml"
      let desc = head . fromRight' $ stackParams
      let logMessage = logExecution Create (_stackName desc)
      logMessage `shouldBe`
           "\nExecuting "
        <> "Create"
        <> " on "
        <> "Stack1"
        <> "...\n"

    it "can generate a stack outputs log message" $ do
      let stackOutputs = Just ([
              (StackOutputName (StackName "myStack") "myOutput1", "OutputValue1")
            , (StackOutputName (StackName "myStack") "myOutput2", "OutputValue2")
            ])
      let logMessage = logStackOutputs stackOutputs
      logMessage `shouldBe`
           "Stack outputs:\n"
        <> "Stack name: myStack, Output name: myOutput1, Output value: OutputValue1\n"
        <> "Stack name: myStack, Output name: myOutput2, Output value: OutputValue2\n"

    it "logs properly when there are no stack outputs" $ do
      let stackOutputs = Nothing
      let logMessage = logStackOutputs stackOutputs
      logMessage `shouldBe` "Stack outputs: None"

  context "filtering log messages" $
    it "can filter a message based on a given prefix" $ do
      let prefix = "Log:"
      let message = "Log: This is a logMessage."
      prefix `shouldSatisfy` filterBuilderBy message
