{-# LANGUAGE LambdaCase #-}
module S3( uploadFileOrFolder
         , getFilesFromFolder
         , uploadBucketFiles
         , makeAltPath
         ) where

import           Conduit ( runConduitRes
                         , sourceDirectoryDeep
                         , sinkList
                         )
import           Control.Exception.Safe (throwM)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.AWS (send, AWSConstraint)
import           Data.Conduit ((.|))
import           Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack, Text)
import           Network.AWS ( chunkedFile
                             , defaultChunkSize
                             )
import qualified Network.AWS.S3 as S3
import           Network.AWS.S3.Types (BucketName(..))
import           System.Directory ( doesDirectoryExist
                                  , doesFileExist
                                  )
import           System.FilePath.Posix ((</>), addTrailingPathSeparator)

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
uploadFileToS3 bucketName@(BucketName name) filePath altFilePath = do
  let key = S3.ObjectKey altFilePath
  body <- chunkedFile defaultChunkSize (unpack filePath)
  let req = S3.putObject bucketName key body
  liftIO . logEvaporate $
       "Uploading " <> filePath
    <> " as "       <> altFilePath
    <> " to "       <> name
  void . send $ req

uploadFolderToS3 :: AWSConstraint r m => BucketName -> Text -> Text -> m ()
uploadFolderToS3 bucketName folderPath altFolderPath = do
  folderFiles <- liftIO . getFilesFromFolder $ unpack folderPath
  let altFolderFiles = makeAltPath (unpack altFolderPath) (unpack folderPath) <$> folderFiles
  traverse_ (uncurry (S3.uploadFileToS3 bucketName)) $ zip (pack <$> folderFiles) altFolderFiles

getFilesFromFolder :: FilePath -> IO [FilePath]
getFilesFromFolder folderPath = runConduitRes $
  sourceDirectoryDeep True folderPath
  .| sinkList

makeAltPath :: FilePath -> FilePath -> FilePath -> Text
makeAltPath altFolderPath folderPath filePath = do
  let prefixPathLength = length (addTrailingPathSeparator folderPath)
  let suffixPath = drop prefixPathLength filePath
  pack $ addTrailingPathSeparator altFolderPath </> suffixPath
