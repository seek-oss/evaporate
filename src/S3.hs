{-# LANGUAGE LambdaCase #-}
module S3(uploadBucketFiles) where

import           Control.Exception.Safe (throwM)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.AWS (send, AWSConstraint)
import           Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack, Text)
import           Network.AWS ( chunkedFile
                             , defaultChunkSize
                             )
import qualified Network.AWS.S3 as S3
import           Network.AWS.S3.Types (BucketName(..))
import           System.Directory ( listDirectory
                                  , doesDirectoryExist
                                  , doesFileExist)
import           System.FilePath (joinPath)

import           Logging (logEvaporate)
import           StackParameters (BucketFiles(..))
import           Types (FileOrFolderDoesNotExist(..))

uploadBucketFiles :: AWSConstraint r m => BucketFiles -> m ()
uploadBucketFiles bucketFiles =
  traverse_ uploadFileOrFolder (flattenBucketFiles bucketFiles)
  where
    flattenBucketFiles :: BucketFiles -> [(BucketName, Text, Text)]
    flattenBucketFiles BucketFiles{..} =
      tupleToTriple <$> staticMap _bucketName (HashMap.toList _paths)

    staticMap :: a -> [b] -> [(a, b)]
    staticMap a = fmap (a,)

    tupleToTriple :: (a, (b, c)) -> (a, b, c)
    tupleToTriple (a, (b, c)) = (a, b, c)

uploadFileOrFolder :: AWSConstraint r m => (BucketName, Text, Text) -> m ()
uploadFileOrFolder (bucketName, path, altPath) =
  (liftIO . doesDirectoryExist) (unpack path) >>= \case
    True  -> uploadFolderToS3 bucketName path altPath
    False -> (liftIO . doesFileExist) (unpack path) >>= \case
      True -> uploadFileToS3 bucketName path altPath
      False -> throwM $ FileOrFolderDoesNotExist path

uploadFileToS3 :: AWSConstraint r m => BucketName -> Text -> Text -> m ()
uploadFileToS3 bucketName@(BucketName name) filePath altPath = do
  let key = S3.ObjectKey altPath
  body <- chunkedFile defaultChunkSize (unpack filePath)
  let req = S3.putObject bucketName key body
  liftIO . logEvaporate $
       "Uploading " <> filePath
    <> " as "       <> altPath
    <> " to "       <> name
  void . send $ req

uploadFolderToS3 :: AWSConstraint r m => BucketName -> Text -> Text -> m ()
uploadFolderToS3 bucketName folderPath altPath = do
  files <- liftIO . listDirectory $ unpack folderPath
  let folderPaths = (pack . joinPath . (\x -> [unpack folderPath, x])) <$> files
  let altPaths = (pack . joinPath . (\x -> [unpack altPath, x])) <$> files
  traverse_ (uncurry (S3.uploadFileToS3 bucketName)) $ zip folderPaths altPaths
