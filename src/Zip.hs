{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Zip( inlineZips
          , writeZips
          , writeZip
          , writeFileOrFolderToZip
          , getPathInArchive
          ) where

import           Codec.Archive.Zip (ZipOption(..), addFilesToArchive, emptyArchive, fromArchive)
import           Control.Exception.Safe (MonadThrow, catch, throwM)
import           Control.Lens ((&), (.~))
import           Control.Monad (foldM, void, when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource(..), register)
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as HashMap
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, unpack)
import           Data.Tuple.Extra ((***))
import           System.Directory (removeFile)
import           System.FilePath
                  (addTrailingPathSeparator, dropTrailingPathSeparator, takeDirectory,
                  takeFileName)
import           System.IO.Error (isDoesNotExistError)

import           FileSystem (checkPath, getFilesFromFolder)
import           Logging (logEvaporate, logZip)
import           StackParameters (BucketFiles(..), paths)
import           Types (PathType(..))

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

writeZips :: (MonadIO m, MonadResource m, MonadThrow m) => BucketFiles -> m ()
writeZips BucketFiles{..} =
  when _isZipped $ do
    let filesList = HashMap.keys _paths
    traverse_ writeZip filesList

writeZip :: (MonadThrow m, MonadResource m) => Text -> m ()
writeZip pathToFile = do
  let stringPath = dropTrailingPathSeparator . unpack $ pathToFile
  let nameOfZip = takeFileName stringPath <> ".zip"
  void . register $ deleteFileIfExists nameOfZip
  checkPath stringPath >>= \case
    File      f -> liftIO $ writeFileOrFolderToZip nameOfZip (takeDirectory f) [f]
    Directory d -> liftIO $ getFilesFromFolder d >>= writeFileOrFolderToZip nameOfZip d

writeFileOrFolderToZip :: FilePath -> FilePath -> [FilePath] -> IO ()
writeFileOrFolderToZip nameOfZip rootPath filePaths = do
  archive <- foldM folder emptyArchive filePaths
  logEvaporate $ logZip nameOfZip filePaths
  BS.writeFile nameOfZip (fromArchive archive)
  where
    folder archive path = do
      let zipPath = getPathInArchive rootPath path
      addFilesToArchive [OptLocation zipPath False] archive [path]

getPathInArchive :: FilePath -> FilePath -> FilePath
getPathInArchive rootPath (takeDirectory -> pathDirectory) =
  let pathDirectory' = if pathDirectory == "." then "" else pathDirectory
  in if rootPath /= ""
    then fromMaybe "" $ stripPrefix (addTrailingPathSeparator rootPath) pathDirectory'
    else pathDirectory'
