module ExternalValuesSpec (main, spec) where

import           Control.Lens ((&), (?~))
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.AWS (Credentials(..), newEnv)
import           Network.AWS.Auth (AccessKey(..), SecretKey(..))
import           Network.AWS.CloudFormation (oOutputKey, oOutputValue, output)
import           Network.AWS.CloudFormation.Types (OnFailure(..))
import           Network.AWS.S3.Types (BucketName(..))
import           Test.Hspec (Spec, context, describe, hspec, it)
import           Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)

import           ExternalValues
                  (Context(..), EnvironmentValueNotFound(..), InvalidBucketName(..),
                  StackOutputNotFound(..), inlineBucketNames, loadPV,
                  mapStackOutputsToExternalValues)
import           StackParameters (BucketFiles(..), ParameterValue(..))
import           Types (HashNotFound(..), StackName(..), StackOutputName(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ExternalValuesSpec" $ do
  context "creating external values" $ do
    it "can map stack outputs to external values" $ do
      let stackName = StackName { getStackName = "myStack" }
      let testOutputs = [
                (output & oOutputKey ?~ "SomeKey" & oOutputValue ?~ "SomeValue")
              , (output & oOutputKey ?~ "SomeOtherKey" & oOutputValue ?~ "SomeOtherValue")
            ]
      let externalValues = [
                (StackOutputName (StackName "myStack") "SomeKey", "SomeValue")
              , (StackOutputName (StackName "myStack") "SomeOtherKey", "SomeOtherValue")
            ]
      mapStackOutputsToExternalValues stackName testOutputs `shouldBe` externalValues

    it "ignores outputs with Nothing as their key or value" $ do
      let stackName = StackName { getStackName = "myStack" }
      let testOutputs = [
                (output & oOutputValue ?~ "SomeValue")
              , (output & oOutputKey ?~ "SomeOtherKey")
              , (output & oOutputKey ?~ "AnotherKey" & oOutputValue ?~ "AnotherValue")
            ]
      let externalValues = [
                (StackOutputName (StackName "myStack") "AnotherKey", "AnotherValue")
            ]
      mapStackOutputsToExternalValues stackName testOutputs `shouldBe` externalValues

  context "inlining bucket names" $ do
    it "can replace a bucket name with a stack output value" $
      withTestContext >>= \testContext -> do
        let bucketFiles = [
              BucketFiles {
                  _bucketName = BucketName "${stack.myStack.output.BucketName}"
                , _isHashed   = False
                , _isZipped   = False
                , _paths      = [("somefile.txt", "folder/somefile.txt")]
                }
              ]
        let inlinedBucketFiles = [
              BucketFiles {
                  _bucketName = BucketName "someBucket"
                , _isHashed   = False
                , _isZipped   = False
                , _paths      = [("somefile.txt", "folder/somefile.txt")]
                }
              ]
        result <- runReaderT (inlineBucketNames bucketFiles) testContext
        result `shouldBe` inlinedBucketFiles

    it "throws if the bucket name is an environment variable reference" $
      withTestContext >>= \testContext -> do
        let bucketFiles = [
              BucketFiles {
                  _bucketName = BucketName "${env.MY_ENVAR}"
                , _isHashed   = False
                , _isZipped   = False
                , _paths      = [("somefile.txt", "folder/somefile.txt")]
                }
              ]
        runReaderT (inlineBucketNames bucketFiles) testContext `shouldThrow`
          (== InvalidBucketName "${env.MY_ENVAR}")

    it "throws if the bucket name is a file hash reference" $
      withTestContext >>= \testContext -> do
        let bucketFiles = [
              BucketFiles {
                  _bucketName = BucketName "${hash.path/to/file.txt}"
                , _isHashed   = False
                , _isZipped   = False
                , _paths      = [("somefile.txt", "folder/somefile.txt")]
                }
              ]
        runReaderT (inlineBucketNames bucketFiles) testContext `shouldThrow`
          (== InvalidBucketName "${hash.path/to/file.txt}")

  context "loading parameter values" $ do
    it "can convert a SimpleValue to text" $ withTestContext >>=
      \testContext -> do
        let parameterValue = SimpleValue "testValue"
        let loadedParameter = "testValue"
        result <- runReaderT (loadPV parameterValue) testContext
        result `shouldBe` loadedParameter

    it "can load an envar from the environment" $ withTestContext >>=
      \testContext -> do
        let parameterValue = EnvVarName "MY_ENVAR"
        let loadedParameter = "abcd1234"
        result <- runReaderT (loadPV parameterValue) testContext
        result `shouldBe` loadedParameter

    it "can load a hash from the environment" $ withTestContext >>=
      \testContext -> do
        let parameterValue = HashFilePath "path/to/file.txt"
        let loadedParameter = "hashwxyz9876"
        result <- runReaderT (loadPV parameterValue) testContext
        result `shouldBe` loadedParameter

    it "can load a stack output from the environment" $ withTestContext >>=
      \testContext -> do
        let parameterValue = StackOutput $ StackOutputName (StackName "myStack") "OutputName"
        let loadedParameter = "outputValue"
        result <- runReaderT (loadPV parameterValue) testContext
        result `shouldBe` loadedParameter

    it "throw if an environment variable can not be found" $ withTestContext >>=
      \testContext -> do
        let parameterValue = EnvVarName "ANOTHER_ENVAR"
        runReaderT (loadPV parameterValue) testContext `shouldThrow`
          (== EnvironmentValueNotFound "ANOTHER_ENVAR")

    it "throw if a hash can not be found" $ withTestContext >>=
      \testContext -> do
        let parameterValue = HashFilePath "someFile.txt"
        runReaderT (loadPV parameterValue) testContext `shouldThrow`
          (== HashNotFound "someFile.txt")

    it "throw if a stack output can not be found" $ withTestContext >>=
      \testContext -> do
        let parameterValue = StackOutput $ StackOutputName (StackName "myStack") "AnotherOutput"
        runReaderT (loadPV parameterValue) testContext `shouldThrow`
          (== (StackOutputNotFound $ StackOutputName (StackName "myStack") "AnotherOutput"))

withTestContext :: IO Context
withTestContext = do
  let systemEnv = [("MY_ENVAR", "abcd1234")]
  let stackOutputs = [
          (StackOutputName (StackName "myStack") "OutputName", "outputValue")
        , (StackOutputName (StackName "myStack") "BucketName", "someBucket")
        ]
  let onCreateFailure = Rollback
  let fileHashes = [("path/to/file.txt", "hashwxyz9876")]
  -- Useless keys since we're not actually making any AWS calls here but we
  -- still have to create an environment
  awsEnv <- newEnv $ FromKeys (AccessKey "Test") (SecretKey "Test")
  return $ Context systemEnv stackOutputs onCreateFailure fileHashes awsEnv
