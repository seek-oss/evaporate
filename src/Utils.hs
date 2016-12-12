module Utils where

import Conduit (runConduitRes, sourceDirectoryDeep, sinkList)
import Data.Conduit ((.|))

getFilesFromFolder :: FilePath -> IO [FilePath]
getFilesFromFolder folderPath = runConduitRes $
  sourceDirectoryDeep True folderPath
  .| sinkList
