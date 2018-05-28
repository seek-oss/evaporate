{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Evaporate where

import           Control.Exception.Safe ( throwM
                                        , handle
                                        , SomeException
                                        , MonadThrow
                                        , Exception(..)
                                        )
import           Control.Lens ((&), (<&>), (.~), (<>~), (^.), (%~), view)
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (asks, local, runReaderT, MonadReader)
import           Control.Monad.Trans.AWS ( runResourceT
                                         , runAWST
                                         , AWSConstraint
                                         , Credentials(Discover)
                                         )
import           Control.Monad.Trans.Resource (MonadBaseControl, MonadResource, liftResourceT)
import           Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (pack, Text)
import qualified Data.Text as Text
import           Data.Tuple.Extra (both)
import           Network.AWS (newEnv, envLogger, envRegion, Env)
import           Network.AWS.CloudFormation (Parameter, Tag)
import qualified Network.AWS.CloudFormation.Types as CFN
import           Network.AWS.Types (LogLevel(..), Region)
import           Network.AWS.Waiter (Accept(..))
import           System.Environment (getEnvironment)
import           System.IO (stdout)
import           System.Log.Logger (setLevel, updateGlobalLogger, Priority(..))
import           Text.Nicify (nicify)

import           Configuration (Command(..), Options(..))
import           ExternalValues ( inlineBucketNames
                                , loadParameterValues
                                , getStackOutputValues
                                , cStackOutputs
                                , cFileHashes
                                , cEnv
                                , Context(..)
                                )
import           Logging ( logMain
                         , logExecution
                         , logEvaporate
                         , logStackOutputs
                         , customLogger
                         , LogParameters(..)
                         )
import           Hash (hashBucketFiles, inlineHashes)
import qualified S3
import qualified Stack
import           StackDependency ( determineStackOrdering
                                 , makeStackDependencyGraph
                                 )
import           StackParameters ( getStackParameters
                                 , getParameters
                                 , convertToStackParameters
                                 , convertToTags
                                 , Capabilities(..)
                                 , BucketFiles
                                 , StackDescription(..)
                                 )
import           STS (getAccountID)
import           Types ( StackOutputs
                       , AWSAccountID
                       , StackOutputLoadFailed(..)
                       , EvaporateException(..)
                       , StackName(..)
                       )
import           Zip (inlineZips, writeZips)

exceptionHandler :: (MonadIO m, MonadThrow m)
                 => LogLevel
                 -> SomeException
                 -> m a
exceptionHandler logLevel e = do
  when (logLevel == Trace) (logEvaporate . pack . show $ e)
  logEvaporate $ (pack . displayException) e
  throwM $ EvaporateException "Fatal"

execute :: Options -> IO ()
execute Options{..} = do
  updateGlobalLogger "EvaporateLogger" (setLevel INFO)
  amazonLogger      <- customLogger logLevel stdout ["[Await"]
  awsEnv            <- newEnv Discover
                       <&> envLogger .~ amazonLogger
                       <&> setRegion defaultRegion
  stackDescriptions <- getStackParameters configFilePath
  systemEnv         <- HashMap.fromList . fmap (both pack) <$> getEnvironment
  let context = Context systemEnv mempty onCreateFailure mempty awsEnv
  handle (exceptionHandler logLevel) . runResourceT $ do
    awsAccountID <- runAWST context $ getAccountID
    stackDependencyGraph <- makeStackDependencyGraph stackDescriptions awsAccountID
    orderedStackDescriptions <- determineStackOrdering stackDependencyGraph
    logEvaporate . logMain . LogParameters
      command orderedStackDescriptions awsAccountID $ awsEnv ^. envRegion
    if isDryRun then
      logEvaporate "Dry run enabled. No commands will be executed."
    else
      void $ processStacks context command awsAccountID orderedStackDescriptions stackNameOption

processStacks :: (MonadResource m, MonadBaseControl IO m, MonadThrow m)
              => Context
              -> Command
              -> AWSAccountID
              -> [StackDescription]
              -> Maybe StackName
              -> m StackOutputs
processStacks context comm accountID stackDescriptions stackNameOption =
  processEachStack filteredStacks `runReaderT` context
  where
    filteredStacks :: [StackDescription]
    filteredStacks = stackDescriptions & maybe id (filter . byStackName) stackNameOption

    processEachStack ::
      ( MonadResource m
      , MonadBaseControl IO m
      , MonadThrow m
      , MonadReader Context m)
      => [StackDescription]
      -> m StackOutputs
    processEachStack [] = asks _cStackOutputs
    processEachStack (stackDescription@StackDescription{..} : otherStacks) = do
      fileHashes <- HashMap.unions <$> traverse hashBucketFiles _s3upload
      local (cFileHashes .~ fileHashes) $ do
        newOutputs <-
          liftResourceT . runAWST (context & cEnv %~ setRegion _region) $
            processStack comm accountID stackDescription
        local (cStackOutputs <>~ newOutputs) $ processEachStack otherStacks

    byStackName :: StackName -> StackDescription -> Bool
    byStackName name StackDescription{..} = _stackName == name

processStack :: forall m. (AWSConstraint Context m)
             => Command
             -> AWSAccountID
             -> StackDescription
             -> m StackOutputs
processStack command accountID stackDescription@StackDescription{..} = do
  region <- asks $ view $ cEnv . envRegion
  logEvaporate $ logExecution command _stackName region
  case command of
    Check  -> do
      Stack.describeStack _stackName >>= liftIO . putStrLn . nicify . show
      return mempty
    Get    -> do
      Stack.listStackResources _stackName >>= liftIO . putStrLn . nicify . show
      return mempty
    Delete -> do
      Stack.deleteStack _stackName >>= Stack.throwIfStackFailed
      return mempty
    Create -> do
      writeToStack stackDescription accountID Stack.createStack >>= Stack.throwIfStackFailed
      outputs <- getStackOutputValues _stackName
      liftIO . logEvaporate . logStackOutputs $ outputs
      maybe (throwM $ StackOutputLoadFailed _stackName) return outputs
    Upsert -> do
      writeToStack stackDescription accountID Stack.upsertStack >>= Stack.throwIfStackFailed
      outputs <- getStackOutputValues _stackName
      liftIO . logEvaporate . logStackOutputs $ outputs
      maybe (throwM $ StackOutputLoadFailed _stackName) return outputs
  where
    writeToStack :: StackDescription
                 -> AWSAccountID
                 -> (CFN.OnFailure -> [CFN.Capability] -> StackName -> Text -> [Parameter] -> [Tag] -> m Accept)
                 -> m Accept
    writeToStack StackDescription{..} accountID' action = do
      fileHashes   <- asks _cFileHashes
      inlinedFiles <- traverse (inlineHashes fileHashes) _s3upload
      let inlinedZipFiles = inlineZips <$> inlinedFiles
      inlinedBucketFiles <- inlineBucketNames inlinedZipFiles
      traverse_ writeZips _s3upload
      uploadToS3 inlinedBucketFiles
      cfContent       <- readFileText _templatePath
      accountParams   <- getParameters _parameters accountID'
      parameters      <- convertToStackParameters <$> loadParameterValues accountParams
      onCreateFailure <- asks _cOnCreateFailure
      action onCreateFailure (getCapabilities _capabilities) _stackName cfContent parameters $ convertToTags _tags
    uploadToS3 :: [BucketFiles] -> m ()
    uploadToS3 = traverse_ S3.uploadBucketFiles

readFileText :: MonadIO m => Text -> m Text
readFileText filePath =
  pack <$> (liftIO . readFile $ Text.unpack filePath)

setRegion :: Maybe Region -> Env -> Env
setRegion = \case
  Nothing -> id
  Just region -> envRegion .~ region
