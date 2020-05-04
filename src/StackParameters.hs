{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module StackParameters where

import           Control.Applicative ((<|>))
import           Control.Exception.Safe (MonadThrow, throwM)
import           Control.Lens ((&), (?~))
import           Control.Lens.TH (makeLenses)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson
                  (FromJSON(..), Value(..), withArray, withObject, withText, (.!=), (.:), (.:?))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text, pack, unpack)
import           Data.Tuple.Extra (dupe)
import           Data.Vector (toList)
import qualified Data.Yaml as Y
import           Network.AWS.CloudFormation
                  (Capability, Parameter, Tag, pParameterKey, pParameterValue, parameter, tag)
import           Network.AWS.Data.Text (fromText)
import           Network.AWS.S3.Types (BucketName(..))
import           Network.AWS.Types (Region)
import           Text.Parser.Token (braces)
import           Text.Trifecta
                  (Parser, Result(..), char, eof, parseString, satisfy, some, text, (<?>))

import           Types
                  (AWSAccountID, EnvironmentNotFound(..), LoadedParameters, Paths, StackName(..),
                  StackOutputName(..), Tags)

type Environments = HashMap AWSAccountID Parameters
type Parameters   = HashMap Text ParameterValue

data ParameterValue = SimpleValue Text
    | EnvVarName Text
    | HashFilePath Text
    | StackOutput StackOutputName
    deriving (Show, Eq)

data BucketFiles = BucketFiles
    { _bucketName :: BucketName
    , _isHashed   :: Bool
    , _isZipped   :: Bool
    , _paths      :: Paths
    }
    deriving (Show, Eq)

newtype Capabilities = Capabilities { getCapabilities :: [Capability] }
  deriving (Show, Eq, Semigroup, Monoid)

data StackDescription = StackDescription
    { _capabilities :: Capabilities
    , _stackName    :: StackName
    , _s3upload     :: [BucketFiles]
    , _templatePath :: Text
    , _tags         :: Tags
    , _parameters   :: Environments
    , _region       :: Maybe Region
    }
    deriving (Show, Eq)

makeLenses ''BucketFiles

instance {-# OVERLAPPING #-} FromJSON [StackDescription] where
  parseJSON = withObject "List of Stack Descriptions" $
    traverse (uncurry parseStack) . HashMap.toList
    where
      parseStack :: Text -> Value -> Y.Parser StackDescription
      parseStack name = withObject ("Stack Description for " <> unpack name) $
        \o -> StackDescription
              <$> o .:? "capabilities" .!= mempty
              <*> pure (StackName name)
              <*> o .:? "s3upload" .!= mempty
              <*> o .:  "template-path"
              <*> o .:? "tags" .!= mempty
              <*> o .:? "parameters" .!= mempty
              <*> o .:? "region" .!= Nothing

-- | Leaning on the Amazonka FromText instance for the Capability type
-- but newtype wrapping into Capabilities to slightly weaken the coupling
instance FromJSON Capabilities where
  parseJSON =
    withArray "Capabilities" $ fmap (Capabilities . toList) . traverse g
    where g :: Value -> Y.Parser Capability
          g = withText "Capability" $ either fail pure . fromText

instance FromJSON BucketFiles where
  parseJSON = withObject "BucketFiles" $ \properties ->
    BucketFiles
    <$> (BucketName <$> properties .: "bucket-name")
    <*> properties .:? "hash" .!= False
    <*> properties .:? "zip" .!= False
    <*> (parsePathsFromObject (properties .: "paths")
        <|> parsePathsFromArray (properties .: "paths"))
    where
      parsePathsFromObject :: Y.Parser (HashMap Text (Maybe Text)) -> Y.Parser Paths
      parsePathsFromObject object =
        object >>= HashMap.traverseWithKey disallowEmptyString

      disallowEmptyString :: Text -> Maybe Text -> Y.Parser Text
      disallowEmptyString key value = case value of
        Just "" -> fail $ unpack "Invalid alternate path: must be null or non-empty"
        Nothing -> return key
        Just v  -> return v

      parsePathsFromArray :: Y.Parser [Text] -> Y.Parser Paths
      parsePathsFromArray = fmap $ HashMap.fromList . fmap dupe

getStackParameters :: (MonadIO m, MonadThrow m) => FilePath -> m [StackDescription]
getStackParameters = liftIO . Y.decodeFileEither >=> either throwM pure

instance FromJSON ParameterValue where
  parseJSON =
    withText "Parameter values must be Text" p
    where
      p :: Text -> Y.Parser ParameterValue
      p input = unpack input & parseString parameterValue mempty & \case
                Success s -> return s
                Failure _ -> fail . unpack $ input <> " is not a valid value"

parameterValue :: Parser ParameterValue
parameterValue = dynamicValue <|> simpleValue

dynamicValue :: Parser ParameterValue
dynamicValue =
  withinCurlyBracePair $ envVarName
                  <|> hashFilePath
                  <|> stackOutputName

withinCurlyBracePair :: Parser a -> Parser a
withinCurlyBracePair x = char '$' *> braces x <* eof

simpleValue :: Parser ParameterValue
simpleValue = SimpleValue <$> innerText <* eof

delimiter :: Parser Char
delimiter = char '.'

innerText :: Parser Text
innerText = pack <$> some (satisfy $ not . isBraces)

isBraces :: Char -> Bool
isBraces c = c == '{' || c == '}'

envVarName :: Parser ParameterValue
envVarName = EnvVarName <$>
  (  (text "env" <?> "env prefix")
  *> delimiter
  *> innerText <?> "environment variable name")

hashFilePath :: Parser ParameterValue
hashFilePath = HashFilePath <$>
  (  (text "hash" <?> "hash prefix")
  *> delimiter
  *> innerText <?> "file path")

stackOutputName :: Parser ParameterValue
stackOutputName = StackOutput <$>
  (StackOutputName . StackName . pack <$>
  (    (text "stack" <?> "stack prefix")
    *> delimiter
    *> some (satisfy $ \c -> (not . isBraces $ c) && c /= '.') <?> "stack name"
    )
  <*>
    (  delimiter
    *> (text "output" <?> "output prefix")
    *> delimiter
    *> innerText <?> "output name")
    )

getParameters :: MonadThrow m => Environments -> AWSAccountID -> m Parameters
getParameters envs acc
  | HashMap.null envs                  = pure mempty
  | Just ps <- HashMap.lookup acc envs = pure ps
  | otherwise                          = throwM $ EnvironmentNotFound acc

convertToStackParameters :: LoadedParameters -> [Parameter]
convertToStackParameters = fmap makeParameter . HashMap.toList
  where makeParameter (key, value) =
          parameter & pParameterKey ?~ key
                    & pParameterValue ?~ value

convertToTags :: Tags -> [Tag]
convertToTags = fmap makeTag . HashMap.toList
  where makeTag (key, value) = tag key value
