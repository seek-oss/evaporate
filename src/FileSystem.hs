{-# LANGUAGE RankNTypes #-}
module FileSystem
  ( checkPath
  , getFilesFromFolder
  , sourceFileOrDirectory
  ) where

import           Conduit (ConduitT, runConduitRes, sinkList, sourceDirectoryDeep, yield)
import           Control.Exception.Safe (throwM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit ((.|))
import           Data.Text (pack)
import           System.Directory (doesDirectoryExist, doesFileExist)

import           Types (FileOrFolderDoesNotExist(..), PathType(..))

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

-- | Deeply stream the contents of the given directory.
-- This works the same as sourceDirectoryDeep, except if the path is a File not a Directory then
-- it will yield just that file.
sourceFileOrDirectory :: MonadResource m => Bool -> FilePath -> ConduitT i FilePath m ()
sourceFileOrDirectory followSymlinks path = do
  fileType <- checkPath path
  case fileType of
    File      f -> yield f
    Directory d -> sourceDirectoryDeep followSymlinks d
