module Utils where

import Conduit (runConduitRes, sourceDirectoryDeep, sinkList)
import Control.Exception.Safe (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Conduit ((.|))
import Data.Text (pack)
import System.Directory (doesDirectoryExist, doesFileExist)

import Types (PathType(..), FileOrFolderDoesNotExist(..))

getFilesFromFolder :: FilePath -> IO [FilePath]
getFilesFromFolder folderPath = runConduitRes $
  sourceDirectoryDeep True folderPath
  .| sinkList

checkPath :: MonadIO m => FilePath -> m PathType
checkPath path = liftIO $ do
  isFile <- doesFileExist path
  if isFile then return $ File path
  else do
    isDir <- doesDirectoryExist path
    if isDir then return $ Directory path
    else throwM . FileOrFolderDoesNotExist . pack $ path
