module StackParametersSpec (spec, main) where

import Control.Lens ((&), (?~))
import Control.Lens.Extras (is)
import Control.Monad.Except (runExceptT)
import Data.Either.Combinators (fromRight')
import Data.Text (Text)
import Data.Yaml (ParseException)
import Network.AWS.CloudFormation ( parameter
                                  , pParameterKey
                                  , pParameterValue
                                  , tag
                                  , tagKey
                                  , tagValue
                                  , Capability(..)
                                  )
import Network.AWS.S3.Types (BucketName(..))
import Test.Hspec ( describe
                  , context
                  , it
                  , hspec
                  , Selector
                  , Spec
                  )
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow, shouldSatisfy)
import Text.Trifecta (parseString, _Failure, Result(..))

import StackParameters ( getStackParameters
                       , withinCurlyBracePair
                       , envVarName
                       , hashFilePath
                       , stackOutputName
                       , innerText
                       , convertToStackParameters
                       , convertToTags
                       , getParameters
                       , BucketFiles(..)
                       , StackDescription(..)
                       , ParameterValue(..)
                       , Capabilities(..)
                       )
import Types (EnvironmentNotFound(..), StackName(..), StackOutputName(..))

main :: IO ()
main = hspec spec

anyParseException :: Selector ParseException
anyParseException = const True

spec :: Spec
spec = describe "StackParametersSpec" $ do
  context "loading parameters" $
    it "can convert from Parameters to AWS Parameter" $ do
      let converted = convertToStackParameters [("a", "value1"), ("b", "value2")]
      converted `shouldBe` [
          (parameter & pParameterKey ?~ "a" & pParameterValue ?~ "value1")
        , (parameter & pParameterKey ?~ "b" & pParameterValue ?~ "value2")
        ]

  context "loading tags" $
    it "can convert from Tags to AWS Tag" $
      convertToTags [("tag1", "value1"), ("tag2", "value2")] `shouldBe` [
          (tag & tagKey ?~ "tag1" & tagValue ?~ "value1")
        , (tag & tagKey ?~ "tag2" & tagValue ?~ "value2")
      ]

  context "getting environment" $ do
    it "can get list of Parameters from Environments and an AWSAccount" $ do
      input <- getParameters [("12345678", [("key1", SimpleValue "value1")])] "12345678"
      let params = [("key1", SimpleValue "value1")]
      input `shouldBe` params

    it "throws if the account can not be found" $
      getParameters [("87654321", [("key1", SimpleValue "value1")])] "12123434" `shouldThrow`
        (== EnvironmentNotFound "12123434")

  context "parsing yaml file" $ do
    it "can parse valid.yaml" $ do
      result <- runExceptT . getStackParameters $ "test/valid.yaml"
      fromRight' result `shouldBe` [
        StackDescription { _stackName = StackName "Stack1"
                         , _capabilities = mempty
                         , _s3upload = []
                         , _templatePath = "template.json"
                         , _tags = [
                           ("tag1", "value1")
                          ,("tag2", "value2")
                         ]
                         , _parameters = [
                           ("env1", [("aws-external-id", SimpleValue "abcde12345")])
                          ,("env2", [("aws-external-id", EnvVarName "MY_ENV_VAR")])
                         ]}
       ,StackDescription { _stackName = StackName "Stack2"
                         , _capabilities = mempty
                         , _s3upload = [
                            BucketFiles {
                                _bucketName = BucketName "bucket1"
                              , _isHashed = False
                              , _isZipped = True
                              , _paths = [("evaporate.yaml", "folder1/evaporate.yaml")]
                            }
                         ]
                         , _templatePath = "template2.json"
                         , _tags = []
                         , _parameters = [
                           ("env1", [("aws-external-id", HashFilePath "evaporate.yaml")])
                          ,("env2", [("aws-external-id", StackOutput (StackOutputName (StackName "myStack") "myOutput"))])
                         ]}]

    it "can parse capabilities" $ do
      result <- runExceptT . getStackParameters $ "test/capabilities.yaml"
      fromRight' result `shouldBe` [
        StackDescription { _stackName = StackName "a-stack-with-capability-iam"
                         , _capabilities = Capabilities [CapabilityIAM]
                         , _s3upload = []
                         , _templatePath = "template.json"
                         , _tags = []
                         , _parameters = [] }
        ]

    it "fails parsing invalid1.yaml" $ do
      let result = getStackParameters "test/invalid1.yaml"
      result `shouldThrow` anyParseException

    it "fails parsing invalid2.yaml" $ do
      let result = getStackParameters "test/invalid1.yaml"
      result `shouldThrow` anyParseException

    describe "tags are optional" $
      it "can parse no-tags.yaml" $ do
        result <- runExceptT . getStackParameters $ "test/no-tags.yaml"
        fromRight' result `shouldBe` [
          StackDescription { _stackName = StackName "Stack1"
                           , _capabilities = mempty
                           , _s3upload = [
                              BucketFiles {
                                  _bucketName = BucketName "someBucket"
                                , _isHashed = True
                                , _isZipped = True
                                , _paths = [("evaporate.yaml", "evaporate.yaml")]
                              }
                           ]
                           , _templatePath = "template.json"
                           , _tags = []
                           , _parameters = [
                              ("env2", [("aws-external-id", SimpleValue "lmnop98765")])
                             ,("env1", [("aws-external-id", SimpleValue "abcde12345")])
                             ,("env3", [("aws-external-id", SimpleValue "pqrst45678")])
                           ]}]

    describe "s3uploads are optional" $
      it "can parse no-s3upload.yaml" $ do
        result <- runExceptT . getStackParameters $ "test/no-s3upload.yaml"
        fromRight' result `shouldBe` [
          StackDescription { _stackName = StackName "Stack1"
                           , _capabilities = mempty
                           , _s3upload = []
                           , _templatePath = "template.json"
                           , _tags = [("SomeTag", "someValue")]
                           , _parameters =
                             [("env2", [("aws-external-id", SimpleValue "lmnop98765")])
                             ,("env1", [("aws-external-id", SimpleValue "abcde12345")])
                             ,("env3", [("aws-external-id", SimpleValue "pqrst45678")])
                           ]}]

  context "parsing the surrounding '${' and '}' of a parameter value" $ do
    let parse :: String -> Result Text
        parse = parseString (withinCurlyBracePair innerText) mempty

    it "parses anything surrounded by '${' and '}'" $
      parse "${test}" `shouldParse` "test"

    it "fails if the text doesn't begin with '${'" $
      parse "{test}" & shouldFailParsing

    it "fails if the text doesn't end with '}'" $
      parse "${test" & shouldFailParsing

    it "fails if there is text after the ending '}'" $
      parse "${test}more" & shouldFailParsing

    it "fails if there are nested braces" $
      parse "${foo{}}" & shouldFailParsing

  context "parsing an environment variable reference" $ do
    let parse :: String -> Result ParameterValue
        parse = parseString envVarName mempty

    it "parses a correct environment variable reference" $
      parse "env.MY_ENV_VAR" `shouldParse` EnvVarName "MY_ENV_VAR"

    it "fails if the 'env' prefix is missing" $
      parse "MY_ENV_VAR" & shouldFailParsing

    it "fails if no environment variable name is given" $
      parse "env." & shouldFailParsing

  context "parsing a file hash reference" $ do
    let parse :: String -> Result ParameterValue
        parse = parseString hashFilePath mempty

    it "parses a correct file hash reference" $
      parse "hash.someFile.txt" `shouldParse`
        HashFilePath "someFile.txt"

    it "fails if the 'hash' prefix is missing" $
      parse "someFile.txt" & shouldFailParsing

    it "fails if no file path is given" $
      parse "hash." & shouldFailParsing

  context "parsing a stack output reference" $ do
    let parse :: String -> Result ParameterValue
        parse = parseString stackOutputName mempty

    it "parses a correct stack output reference" $
      parse "stack.myStack.output.MyOutput" `shouldParse`
        StackOutput (StackOutputName (StackName "myStack") "MyOutput")

    it "fails if the 'stack' prefix is missing" $
      parse "myStack.output.MyOutput" & shouldFailParsing

    it "fails if the 'output' prefix is missing" $
      parse "stack.myStack.MyOutput" & shouldFailParsing

    it "fails if no output name is given" $
      parse "stack.myStack.output." & shouldFailParsing

shouldParse :: (Eq a, Show a) => Result a -> a -> IO ()
shouldParse (Success a) expected = a `shouldBe` expected
shouldParse (Failure err) _ = fail . show $ err

shouldFailParsing :: Show a => Result a -> IO ()
shouldFailParsing result = result `shouldSatisfy` is _Failure
