module StackDependencySpec (spec, main) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (runReaderT)
import Network.AWS.S3.Types (BucketName(..))
import Test.Hspec ( describe
                  , context
                  , it
                  , hspec
                  , Spec
                  )
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)

import StackDependency ( makeStackDependencyGraph
                       , parameterValueToDependency
                       , getStackOutputBucketName
                       , determineStackOrdering
                       , StackDependencyGraph
                       , StackNodeMap
                       , StackReference(..)
                       , DependencyCycleDetected(..)
                       , UnknownStackReference(..)
                       )
import StackParameters ( getStackParameters
                       , Capabilities(..)
                       , ParameterValue(..)
                       , StackDescription(..)
                       )
import Types (OutputName(..), StackName(..), StackOutputName(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "StackDependencySpec" $ do
  context "determining the order to execute commands on stacks" $ do
    it "returns a valid ordering if given an acyclic graph" $ do
      -- Getting just the stack names to avoid bloating the test code with
      -- 5 StackDescription declarations
      stackDependencyGraph <- acyclicStackDependencyGraph
      stackOrdering <- determineStackOrdering stackDependencyGraph
      fmap _stackName stackOrdering `shouldBe`
        fmap StackName ["test-stack1", "test-stack3", "test-stack2", "test-stack4", "test-stack5"]

    it "throws if given a graph with a dependency cycle" $ do
      stackDependencyGraph <- cyclicStackDependencyGraph
      determineStackOrdering stackDependencyGraph `shouldThrow`
        (== DependencyCycleDetected [[StackName "test-stack5", StackName "test-stack4", StackName "test-stack3"]])

  context "determining stack dependencies from parameter values" $ do
    let sourceStack = StackDescription mempty (StackName "stack2") mempty mempty mempty mempty
    let sourceStackNode = (1, sourceStack)
    it "returns a dependency if given a StackOutput" $ do
      let testParameterValue = StackOutput (StackOutputName (StackName "stack1") "outputName")
      result <- runReaderT (parameterValueToDependency sourceStackNode testParameterValue) testStackNodeMap
      result `shouldBe` Just (0, 1, OutputName "outputName")

    it "throws if an unknown stack is referenced" $ do
      let unknownStack = StackName "unknown-stack"
      let testStackOutput = StackOutputName unknownStack "outputName"
      let testParameterValue = StackOutput testStackOutput
      runReaderT (parameterValueToDependency sourceStackNode testParameterValue) testStackNodeMap
        `shouldThrow` (== UnknownStackReference (StackReference (_stackName sourceStack) testStackOutput))

    it "returns nothing if given an EnvVarName" $ do
      let testParameterValue = EnvVarName "MY_EVNAR"
      result <- runReaderT (parameterValueToDependency sourceStackNode testParameterValue) testStackNodeMap
      result `shouldBe` Nothing

    it "returns nothing if given an HashFilePath" $ do
      let testParameterValue = HashFilePath "path/to/file.txt"
      result <- runReaderT (parameterValueToDependency sourceStackNode testParameterValue) testStackNodeMap
      result `shouldBe` Nothing

    it "returns nothing if given an SimpleValue" $ do
      let testParameterValue = SimpleValue "myValue"
      result <- runReaderT (parameterValueToDependency sourceStackNode testParameterValue) testStackNodeMap
      result `shouldBe` Nothing

  context "extracting stack outputs from bucket names" $ do
    it "returns a stack output if the bucket name references a stack output" $ do
      let bucketName = BucketName "${stack.myStack.output.myOutput}"
      getStackOutputBucketName bucketName `shouldBe`
        Just (StackOutput (StackOutputName (StackName "myStack") "myOutput"))

    it "returns nothing if the bucket name is normal not a stack output" $ do
      let bucketName = BucketName "myBucket"
      getStackOutputBucketName bucketName `shouldBe` Nothing

cyclicStackDependencyGraph :: (MonadIO m, MonadThrow m) => m StackDependencyGraph
cyclicStackDependencyGraph = do
  stackParameters <- getStackParameters "test/cyclic-dependency.yaml"
  makeStackDependencyGraph stackParameters "123412341234"

acyclicStackDependencyGraph :: (MonadIO m, MonadThrow m) => m StackDependencyGraph
acyclicStackDependencyGraph = do
  stackParameters <- getStackParameters "test/acyclic-dependency.yaml"
  makeStackDependencyGraph stackParameters "123412341234"

testStackNodeMap :: StackNodeMap
testStackNodeMap = [(StackName "stack1", (0, StackDescription (Capabilities []) (StackName "stack1") [] "some-path.yaml" [] []))
                   ,(StackName "stack2", (1, StackDescription (Capabilities []) (StackName "stack2") [] "some-path.yaml" [] []))]
