module Logging where

import           Control.Lens.Indexed (ifoldMap)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.ByteString.Builder (hPutBuilder, toLazyByteString, Builder)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Network.AWS.Data.Text (toText)
import           Network.AWS.S3.Types (BucketName(..))
import           Network.AWS.Types (Logger, Region, LogLevel(..))
import           System.Log.Logger (infoM)
import           System.IO (hSetBinaryMode, hSetBuffering, Handle, BufferMode(..))

import           Configuration (Command(..))
import           StackParameters (StackDescription(..))
import           Types ( AWSAccountID
                       , StackOutputs
                       , StackName(..)
                       , StackOutputName(..)
                       )

type Filters = [Text]

data LogParameters = LogParameters
  { _lpCommand           :: Command
  , _lpStackDescriptions :: [StackDescription]
  , _lpAwsAccountID      :: AWSAccountID
  , _lpRegion            :: Region }

-- General logging function
logEvaporate :: MonadIO m => Text -> m ()
logEvaporate = liftIO . infoM "EvaporateLogger" . unpack

logMain :: LogParameters -> Text
logMain LogParameters{..} =
     logGeneral _lpCommand _lpAwsAccountID _lpRegion
  <> "\nStack(s) being operated on:"
  <> foldMap logStackName _lpStackDescriptions

logStackOutputs :: Maybe StackOutputs -> Text
logStackOutputs (Just stackOutputs) =
  "Stack outputs:\n" <> ifoldMap logStackOutput stackOutputs
  where
    logStackOutput :: StackOutputName -> Text -> Text
    logStackOutput StackOutputName{..} stackOutputValue =
          "Stack name: " <> getStackName _soStackName <> ", "
       <> "Output name: " <> _soOutputName <> ", "
       <> "Output value: " <> stackOutputValue <> "\n"
logStackOutputs Nothing = "Stack outputs: None"

logGeneral :: Command -> AWSAccountID -> Region -> Text
logGeneral command accountID region =
     "\nCommand being executed: "
  <> (pack . show $ command)
  <> "\nAWS Account ID: "
  <> accountID
  <> "\nRegion: "
  <> toText region

logStackName :: StackDescription -> Text
logStackName StackDescription{..} =
  "\n    " <> getStackName _stackName

logExecution :: Command -> StackName -> Region -> Text
logExecution command StackName{..} region =
     "\nExecuting "
  <> (pack . show $ command)
  <> " on "
  <> getStackName
  <> " in "
  <> toText region
  <> "...\n"

logFileUpload :: Text -> Text -> BucketName -> Text
logFileUpload filePath altFilePath (BucketName bucketName) =
     "Uploading " <> filePath
  <> " as "       <> altFilePath
  <> " to "       <> bucketName

logZip :: FilePath -> [FilePath] -> Text
logZip nameOfZip filePaths =
     "Creating archive "
  <> pack nameOfZip
  <> foldMap logFileInZip filePaths
  where
    logFileInZip filePath = "\n    " <> pack filePath

-- Based off the amazonka logger
customLogger :: MonadIO m => LogLevel -> Handle -> Filters -> m Logger
customLogger level handle filters = liftIO $ do
  hSetBinaryMode handle True
  hSetBuffering  handle LineBuffering
  return $ \_ builder ->
    if level > Info
      then hPutBuilder handle (builder <> "\n")
    else
      when (any (filterBuilderBy builder) filters) $
        hPutBuilder handle (builder <> "\n")

filterBuilderBy :: Builder -> Text -> Bool
filterBuilderBy builder word =
  BS.isPrefixOf (BS.fromStrict . encodeUtf8 $ word) (toLazyByteString builder)
