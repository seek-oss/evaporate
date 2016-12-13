{-# LANGUAGE LambdaCase #-}
module S3( uploadFileOrFolder
         , getFilesFromFolder
         , uploadBucketFiles
         , makeS3FilePath
         ) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.AWS (send, AWSConstraint)
import           Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (pack, unpack, Text)
import           Network.AWS ( chunkedFile
                             , defaultChunkSize
                             )
import qualified Network.AWS.S3 as S3
import           Network.AWS.S3.Types (BucketName(..))
import           System.FilePath.Posix ((</>), addTrailingPathSeparator)

import           Logging (logEvaporate, logFileUpload)
import           StackParameters (BucketFiles(..))
import           Types (PathType(..))
import           Utils (getFilesFromFolder, checkPath)

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
  checkPath (unpack path) >>= \case
    File      _ -> uploadFileToS3 bucketName path altPath
    Directory _ -> uploadFolderToS3 bucketName path altPath

uploadFileToS3 :: AWSConstraint r m => BucketName -> Text -> Text -> m ()
uploadFileToS3 bucketName filePath altFilePath = do
  let key = S3.ObjectKey altFilePath
  body <- chunkedFile defaultChunkSize (unpack filePath)
  let req = S3.putObject bucketName key body
  liftIO . logEvaporate $ logFileUpload filePath altFilePath bucketName
  void . send $ req

uploadFolderToS3 :: AWSConstraint r m => BucketName -> Text -> Text -> m ()
uploadFolderToS3 bucketName folderPath altFolderPath = do
  folderFiles <- liftIO . getFilesFromFolder $ unpack folderPath
  let altFolderFiles = makeS3FilePath (unpack altFolderPath) (unpack folderPath) <$> folderFiles
  traverse_ (uncurry (S3.uploadFileToS3 bucketName)) $ zip (pack <$> folderFiles) altFolderFiles

makeS3FilePath :: FilePath -> FilePath -> FilePath -> Text
makeS3FilePath altFolderPath folderPath filePath = do
  let prefixPathLength = length (addTrailingPathSeparator folderPath)
  let suffixPath = drop prefixPathLength filePath
  pack $ addTrailingPathSeparator altFolderPath </> suffixPath
