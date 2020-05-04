{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ExternalValues where

import           Control.Exception.Safe (Exception(..), MonadThrow, throwM)
import           Control.Lens (Iso', each, iso, mapMOf, view, (&), (^.))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Reader (asks)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Trans.AWS (AWSConstraint, Env, HasEnv(..))
import           Data.Hashable (Hashable)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.Maybe (mapMaybe)
import           Data.Text (Text, unpack)
import           Data.Typeable (Typeable)
import           Network.AWS.CloudFormation (Output, oOutputKey, oOutputValue, sOutputs)
import           Network.AWS.CloudFormation.Types (OnFailure)
import           Network.AWS.S3.Types (BucketName(..))
import           Text.Trifecta (Result(..), parseString)

import qualified Stack
import           StackParameters
                  (BucketFiles(..), ParameterValue(..), Parameters, bucketName, parameterValue)
import           Types
                  (FileHashes, HashNotFound(..), LoadedParameters, StackName(..),
                  StackOutputName(..), StackOutputs)

type ExternalValues    = HashMap Text Text
type SystemEnvironment = HashMap Text Text

data Context = Context {
    _cEnvironment     :: SystemEnvironment
  , _cStackOutputs    :: StackOutputs
  , _cOnCreateFailure :: OnFailure
  , _cFileHashes      :: FileHashes
  , _cEnv             :: Env
  }

makeLenses ''Context

instance HasEnv Context where
  environment = cEnv

newtype EnvironmentValueNotFound = EnvironmentValueNotFound Text
  deriving (Eq, Show, Typeable)

instance Exception EnvironmentValueNotFound where
  displayException (EnvironmentValueNotFound e) = unpack $
    "Could not find environment value '" <> e <> "'."

data StackOutputNotFound = StackOutputNotFound StackOutputName
  deriving (Eq, Show, Typeable)

instance Exception StackOutputNotFound where
  displayException (StackOutputNotFound StackOutputName{..}) = unpack $
       "Could not find output '"
    <> _soOutputName <> "' of stack '"
    <> getStackName _soStackName <> "'."

data InvalidBucketName = InvalidBucketName Text
  deriving (Eq, Show, Typeable)

instance Exception InvalidBucketName where
  displayException (InvalidBucketName b) = unpack $
    "Bucket name '" <> b <> "' is invalid. You can only specify strings or \
    \stack output references as bucket names."

getStackOutputValues :: AWSConstraint r m => StackName -> m (Maybe StackOutputs)
getStackOutputValues stackName =
  let stack = Stack.describeStack stackName
  in (fmap . fmap) (mapStackOutputsToExternalValues stackName . view sOutputs) stack

mapStackOutputsToExternalValues :: StackName -> [Output] -> StackOutputs
mapStackOutputsToExternalValues stack =
  HashMap.fromList . mapMaybe stackOutputToExternalValue
  where
    stackOutputToExternalValue :: Output -> Maybe (StackOutputName, Text)
    stackOutputToExternalValue output = do
      key <- output ^. oOutputKey
      value <- output ^. oOutputValue
      return (StackOutputName stack key, value)

inlineBucketNames :: (MonadReader Context m, MonadThrow m)
                  => [BucketFiles]
                  -> m [BucketFiles]
inlineBucketNames =
  (each . bucketName . rawBucketName) `mapMOf` replaceNameFromEnv
  where
    replaceNameFromEnv :: (MonadReader Context m, MonadThrow m) => Text -> m Text
    replaceNameFromEnv t = unpack t & parseString parameterValue mempty & \case
      Success s@(StackOutput _) -> loadPV s
      Success (EnvVarName _) -> throwM $ InvalidBucketName t
      Success (HashFilePath _) -> throwM $ InvalidBucketName t
      Success (SimpleValue _) -> return t
      Failure _ -> throwM $ InvalidBucketName $ t <> " is not a valid value"

rawBucketName :: Iso' BucketName Text
rawBucketName = iso (\(BucketName text) -> text) BucketName

-- This is the new form of converting ParameterValue into their plain form via late-binding

loadParameterValues :: (MonadReader Context m, MonadThrow m)
                    => Parameters
                    -> m LoadedParameters
loadParameterValues = traverse loadPV

loadPV :: (MonadReader Context m, MonadThrow m) => ParameterValue -> m Text
loadPV (SimpleValue s)  = pure s
loadPV (EnvVarName e)   =  asks _cEnvironment >>= lookupOrThrow e EnvironmentValueNotFound
loadPV (HashFilePath h) = asks _cFileHashes >>= lookupOrThrow h HashNotFound
loadPV (StackOutput s)  = asks _cStackOutputs >>= lookupOrThrow s StackOutputNotFound

lookupOrThrow :: (Hashable k, Eq k, Exception e, MonadThrow m)
              => k
              -> (k -> e)
              -> HashMap k v
              -> m v
lookupOrThrow key e dict =
  HashMap.lookup key dict & maybe (throwM $ e key) return
