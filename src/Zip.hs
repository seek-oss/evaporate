{-# LANGUAGE LambdaCase #-}
module Zip( inlineZips
          , writeZips
          , writeZip
          ) where

import           Codec.Archive.Zip ( addFilesToArchive
                                   , emptyArchive
                                   , fromArchive
                                   , ZipOption(..)
                                   )
import           Control.Exception.Safe (catch, throwM, MonadThrow)
import           Control.Monad (when, void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (register, MonadResource(..))
import           Control.Lens ((&), (.~))
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack, Text)
import           Data.Foldable (traverse_)
import           Data.Tuple.Extra ((***))
import           System.Directory ( removeFile
                                  , doesDirectoryExist
                                  , doesFileExist
                                  , listDirectory
                                  )
import           System.FilePath ( takeFileName
                                 , dropTrailingPathSeparator
                                 , joinPath
                                 )
import           System.IO.Error (isDoesNotExistError)

import           Logging (logEvaporate)
import           StackParameters (paths, BucketFiles(..))
import           Types (FileOrFolderDoesNotExist(..))

inlineZips :: BucketFiles -> BucketFiles
inlineZips bucketFiles@BucketFiles{..} =
  if _isZipped
    then do
      let zipFilesList = HashMap.toList _paths
      let inlinedZipFiles = fmap (pathToZipPath *** (<> ".zip")) zipFilesList
      bucketFiles & paths .~ HashMap.fromList inlinedZipFiles
    else bucketFiles
  where
    pathToZipPath :: Text -> Text
    pathToZipPath path =
      pack . takeFileName . dropTrailingPathSeparator $ unpack path <> ".zip"

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists filepath =
  removeFile filepath `catch` handleDoesNotExist
  where
    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise             = throwM e

writeZips :: (MonadIO m, MonadResource m) => BucketFiles -> m ()
writeZips BucketFiles{..} =
  when _isZipped $ do
    let filesList = HashMap.keys _paths
    traverse_ writeZip filesList

writeZip :: (MonadThrow m, MonadResource m) => Text -> m ()
writeZip pathToFile = do
  let stringPath = unpack pathToFile
  let nameOfZip = (takeFileName . dropTrailingPathSeparator $ stringPath) <> ".zip"
  void . register $ deleteFileIfExists nameOfZip
  (liftIO . doesDirectoryExist) stringPath >>= \case
    True -> liftIO . writeFolderToZip stringPath $ nameOfZip
    False -> (liftIO . doesFileExist) stringPath >>= \case
      True -> liftIO . writeFileToZip stringPath $ nameOfZip
      False -> throwM $ FileOrFolderDoesNotExist pathToFile

writeFileToZip :: FilePath -> FilePath -> IO ()
writeFileToZip path nameOfZip = do
  archive <- addFilesToArchive [OptRecursive] emptyArchive [path]
  logEvaporate $ "Zipping " <> pack path
  BS.writeFile nameOfZip (fromArchive archive)

writeFolderToZip :: FilePath -> FilePath -> IO ()
writeFolderToZip path nameOfZip = do
  directoryFiles <- (fmap . fmap) (\x -> [path, x]) (listDirectory path)
  let relativeFilePaths = fmap joinPath directoryFiles
  archive <- addFilesToArchive
    [OptRecursive, OptLocation "." False] emptyArchive relativeFilePaths
  logEvaporate $ "Zipping " <> pack path
  BS.writeFile nameOfZip (fromArchive archive)
