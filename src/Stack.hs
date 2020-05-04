{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack where

import           Control.Applicative (Alternative, empty)
import           Control.Error.Util (hush)
import           Control.Exception.Safe (Exception(..), MonadThrow, throwM)
import           Control.Lens (Traversal', filtered, (&), (.~), (?~), (^.))
import           Control.Monad (void)
import           Control.Monad.Trans.AWS (AWSConstraint, await, send)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Text (Text, unpack)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Network.AWS (catching, trying)
import qualified Network.AWS.CloudFormation as CF
import qualified Network.AWS.CloudFormation.Types as CFN
import           Network.AWS.Types
                  (AWSRequest, AsError(..), Error(ServiceError), ErrorCode(..), ErrorMessage(..),
                  ServiceError(..), serviceCode, serviceMessage)
import           Network.AWS.Waiter (Accept(..), Wait(..))
import           Network.HTTP.Types.Status (statusCode)

import           Logging (logEvaporate)
import           Types (StackName(..))

data MultipleStacksReturned =
  MultipleStacksReturned [CF.Stack]
  deriving (Show, Typeable)

instance Exception MultipleStacksReturned where
  displayException (MultipleStacksReturned s) = unpack $
       "Multiple stacks returned: "
    <> (Text.intercalate ", " . fmap (^. CF.sStackName)) s

data StackOperationFailed =
  StackOperationFailed Accept
  deriving (Show, Typeable)

instance Exception StackOperationFailed where
  displayException (StackOperationFailed s) = "Stack operation failed: " <> show s

throwIfStackFailed :: MonadThrow m => Accept -> m ()
throwIfStackFailed AcceptSuccess = pure ()
throwIfStackFailed a             = throwM $ StackOperationFailed a

sendAndWait :: (AWSConstraint r m, AWSRequest a) => Wait CF.DescribeStacks -> a -> StackName -> m Accept
sendAndWait waiter req StackName{..} = do
  void . send $ req
  await waiter $ CF.describeStacks & CF.dStackName ?~ getStackName

deleteStack :: AWSConstraint r m => StackName -> m Accept
deleteStack stack@StackName{..} =
  sendAndWait CF.stackDeleteComplete (CF.deleteStack getStackName) stack

createStack :: AWSConstraint r m => CFN.OnFailure -> [CFN.Capability] -> StackName -> Text -> [CF.Parameter] -> [CF.Tag] -> m Accept
createStack onCreateFailure capabilities stack@StackName{..} cfContent parameters tags =
  handleStackAlreadyExists $ sendAndWait CF.stackCreateComplete req stack
  where
    req = CF.createStack       getStackName
        & CF.csCapabilities .~ capabilities
        & CF.csParameters   .~ parameters
        & CF.csTags         .~ tags
        & CF.csTemplateBody ?~ cfContent
        & CF.csOnFailure    ?~ onCreateFailure

    handleStackAlreadyExists :: AWSConstraint r m => m Accept -> m Accept
    handleStackAlreadyExists action =
      catching _ServiceError action handleError
      where
        handleError :: AWSConstraint r m => ServiceError -> m Accept
        handleError err =
          case err ^. serviceCode of
            ErrorCode "AlreadyExists" -> do
              logEvaporate $ "Stack " <> getStackName <> " already exists."
              pure AcceptFailure
            _ -> throwM $ ServiceError err

updateStack :: AWSConstraint r m => [CFN.Capability] -> StackName -> Text -> [CF.Parameter] -> [CF.Tag] -> m Accept
updateStack capabilities stack@StackName{..} cfContent parameters tags =
  handleStackAlreadyUpToDate $ sendAndWait CF.stackUpdateComplete req stack
  where
    req = CF.updateStack       getStackName
        & CF.usCapabilities .~ capabilities
        & CF.usParameters   .~ parameters
        & CF.usTags         .~ tags
        & CF.usTemplateBody ?~ cfContent

    handleStackAlreadyUpToDate :: AWSConstraint r m => m Accept -> m Accept
    handleStackAlreadyUpToDate action =
      catching _ServiceError action handleError
      where
        handleError :: AWSConstraint r m => ServiceError -> m Accept
        handleError err =
          case err ^. serviceMessage of
            Just (ErrorMessage "No updates are to be performed.") -> do
              logEvaporate "No updates are to be performed."
              pure AcceptSuccess
            _ -> throwM $ ServiceError err

upsertStack :: AWSConstraint r m => CFN.OnFailure -> [CFN.Capability] -> StackName -> Text -> [CF.Parameter] -> [CF.Tag] -> m Accept
upsertStack onCreateFailure capabilities stackName cfTemplate parameters tags = do
  response <- describeStack stackName
  let action = case response of
        Nothing -> createStack onCreateFailure capabilities
        Just {} -> updateStack capabilities
  action stackName cfTemplate parameters tags

-- | Returns Just the Stack with the given name, or Nothing if none exists
describeStack :: AWSConstraint r m => StackName -> m (Maybe CF.Stack)
describeStack StackName{..} = runMaybeT $ do
  -- When a stack name filter is provided in a describeStacks query, the response will never be
  -- empty or contain more than one stack, but since the describeStacks query can be used to
  -- query for ALL stacks in an account, we have to translate from the larger type to the smaller.
  let req = CF.describeStacks & CF.dStackName ?~ getStackName
  response <- MaybeT . fmap hush . trying (httpError [400]) . send $ req
  response ^. CF.dsrsStacks & assertOneOrEmpty MultipleStacksReturned

listStacks :: AWSConstraint r m => m [CF.StackSummary]
listStacks = do
  response <- send $ CF.listStacks & CF.lsStackStatusFilter .~ [CF.SSCreateComplete, CF.SSUpdateComplete]
  return $ response ^. CF.lsrsStackSummaries

listStackResources :: AWSConstraint r m => StackName -> m [CF.StackResourceSummary]
listStackResources StackName{..} = do
  response <- send $ CF.listStackResources getStackName
  return $ response ^. CF.lsrrsStackResourceSummaries

-- This optic can be used with `trying` to target only the service errors which have
-- an http statuscode which is in the given list of status codes. Other errors will
-- continue to be thrown as usual.
httpError :: AsError a => [Int] -> Traversal' a ServiceError
httpError statusCodes = _ServiceError . filtered ((`elem` statusCodes) . statusCode . _serviceStatus )

assertOneOrEmpty :: (MonadThrow m, Alternative m, Exception e) => ([a] -> e) -> [a] -> m a
assertOneOrEmpty _   [a] = return a
assertOneOrEmpty _   []  = empty
assertOneOrEmpty err as  = throwM $ err as
