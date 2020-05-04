module STS where

import           Control.Exception.Safe (Exception(..), throwM)
import           Control.Lens (view)
import           Control.Monad.Trans.AWS (AWSConstraint, send)
import           Data.Typeable (Typeable)
import qualified Network.AWS.STS as AWSSTS

import           Types (AWSAccountID)

data AccountIDNotFound =
  AccountIDNotFound deriving (Show, Typeable)

instance Exception AccountIDNotFound where
  displayException _ = "Unable to retrieve an Account ID for the current AWS \
  \ caller identity. This should not be possible to fail, please try running \
  \ again with verbose logging enabled (-V) to identify the problem."

getCallerIdentity :: AWSConstraint r m => m AWSSTS.GetCallerIdentityResponse
getCallerIdentity = send AWSSTS.getCallerIdentity

getAccountID :: AWSConstraint r m => m AWSAccountID
getAccountID = do
  accountID <- view AWSSTS.gcirsAccount <$> getCallerIdentity
  case accountID of
    Just a  -> pure a
    Nothing -> throwM AccountIDNotFound
