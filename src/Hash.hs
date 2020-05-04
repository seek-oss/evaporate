{-# LANGUAGE LambdaCase #-}
module Hash( inlineHashes
           , hashBucketFiles
           , createHash
           , FileOrFolderDoesNotExist(..)
           , HashNotFound(..)
           ) where

import           Conduit (awaitForever, runConduitRes, sourceFile)
import           Control.Exception.Safe (MonadThrow, throwM)
import           Control.Lens ((&), (.~))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Crypto.Hash (Digest)
import           Crypto.Hash.Algorithms (SHA1(..))
import           Crypto.Hash.Conduit (sinkHash)
import           Data.Conduit ((.|))
import qualified Data.HashMap.Lazy as HashMap
import           Data.Text (Text, pack, unpack)
import           System.FilePath.Posix (joinPath)

import           FileSystem (sourceFileOrDirectory)
import           StackParameters (BucketFiles(..), paths)
import           Types (FileHashes, FileOrFolderDoesNotExist(..), HashNotFound(..), Paths)

inlineHashes :: MonadThrow m => FileHashes -> BucketFiles -> m BucketFiles
inlineHashes hashedPaths bucketFiles@BucketFiles{..} =
  if _isHashed
    then do
      inlinedFiles <- HashMap.traverseWithKey
        (prependWithHash hashedPaths) _paths
      return $ bucketFiles & paths .~ inlinedFiles
    else return bucketFiles
  where
    prependWithHash :: MonadThrow m => Paths -> Text -> Text -> m Text
    prependWithHash hashes path altPath =
      maybe (throwHashNotFound path)
            (pure . pack . joinPath . (: [unpack altPath]) . unpack)
            (HashMap.lookup path hashes)
      where
        throwHashNotFound filePath = throwM $ HashNotFound filePath

hashBucketFiles :: (MonadThrow m, MonadIO m, MonadUnliftIO m) => BucketFiles -> m FileHashes
hashBucketFiles BucketFiles{..} =
  if _isHashed
    then HashMap.traverseWithKey hashKey _paths
    else return HashMap.empty
  where
    hashKey :: (MonadThrow m, MonadIO m, MonadUnliftIO m) => Text -> a -> m Text
    hashKey path _ = do
      hash <- createHash $ unpack path
      return $ (pack . show) hash

createHash :: (MonadThrow m, MonadIO m, MonadUnliftIO m)
           => FilePath
           -> m (Digest SHA1)
createHash path = runConduitRes $
  sourceFileOrDirectory True path .| awaitForever sourceFile .| sinkHash
