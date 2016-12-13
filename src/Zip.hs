{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Zip( inlineZips
          , writeZips
          , writeZip
          , writeFileOrFolderToZip
          , getPathInArchive
          ) where

import           Codec.Archive.Zip ( addFilesToArchive
                                   , emptyArchive
                                   , fromArchive
                                   , ZipOption(..)
                                   )
import           Control.Exception.Safe (catch, throwM, MonadThrow)
import           Control.Monad (when, void, foldM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (register, MonadResource(..))
import           Control.Lens ((&), (.~))
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack, Text)
import           Data.Foldable (traverse_)
import           Data.Tuple.Extra ((***))
import           System.Directory ( removeFile
                                  , doesDirectoryExist
                                  , doesFileExist
                                  )
import           System.FilePath ( takeFileName
                                 , takeDirectory
                                 , dropTrailingPathSeparator
                                 , addTrailingPathSeparator
                                 )
import           System.IO.Error (isDoesNotExistError)

import           Logging (logEvaporate, logZip)
import           StackParameters (paths, BucketFiles(..))
import           Types (FileOrFolderDoesNotExist(..))
import           Utils (getFilesFromFolder)

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
  let stringPath = dropTrailingPathSeparator . unpack $ pathToFile
  let nameOfZip = takeFileName stringPath <> ".zip"
  void . register $ deleteFileIfExists nameOfZip
  (liftIO . doesDirectoryExist) stringPath >>= \case
    True -> liftIO $
      getFilesFromFolder stringPath >>= writeFileOrFolderToZip nameOfZip stringPath
    False -> (liftIO . doesFileExist) stringPath >>= \case
      True -> liftIO $ writeFileOrFolderToZip nameOfZip (takeDirectory stringPath) [stringPath]
      False -> throwM $ FileOrFolderDoesNotExist pathToFile

writeFileOrFolderToZip :: FilePath -> FilePath -> [FilePath] -> IO ()
writeFileOrFolderToZip nameOfZip rootPath filePaths = do
  archive <- foldM
    (\archive path -> do
      let zipPath = getPathInArchive rootPath path
      addFilesToArchive [OptVerbose, OptLocation zipPath False] archive [path])
    emptyArchive
    filePaths
  logEvaporate $ logZip nameOfZip filePaths
  BS.writeFile nameOfZip (fromArchive archive)

getPathInArchive :: FilePath -> FilePath -> FilePath
getPathInArchive rootPath (takeDirectory -> pathDirectory) =
  let pathDirectory' = if pathDirectory == "." then "" else pathDirectory
  in if rootPath /= ""
    then fromMaybe "" $ stripPrefix (addTrailingPathSeparator rootPath) pathDirectory'
    else pathDirectory'
