{-# LANGUAGE LambdaCase #-}
module Hash( inlineHashes
           , hashBucketFiles
           , createHash
           , FileOrFolderDoesNotExist(..)
           , HashNotFound(..)
           ) where

import           Control.Exception.Safe (throwM, MonadThrow)
import           Control.Lens ((&), (.~))
import           Control.Monad.IO.Class (MonadIO(..))
import           Crypto.Hash ( hashInitWith
                             , hashUpdates
                             , hashFinalize
                             , Digest
                             )
import           Crypto.Hash.Algorithms (SHA1(..))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HashMap
import           Data.Text (pack, unpack, Text)
import           System.Directory ( listDirectory
                                  , doesDirectoryExist
                                  , doesFileExist
                                  )
import           System.FilePath (joinPath)
import qualified System.FilePath.Posix as Posix

import           StackParameters (paths, BucketFiles(..))
import           Types ( Paths
                       , FileHashes
                       , FileOrFolderDoesNotExist(..)
                       , HashNotFound(..)
                       )

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
            (pure . pack . Posix.joinPath . (: [unpack altPath]) . unpack)
            (HashMap.lookup path hashes)
      where
        throwHashNotFound filePath = throwM $ HashNotFound filePath

hashBucketFiles :: (MonadIO m, MonadThrow m) => BucketFiles -> m FileHashes
hashBucketFiles BucketFiles{..} =
  if _isHashed
    then HashMap.traverseWithKey hashKey _paths
    else return HashMap.empty
  where
    hashKey :: (MonadIO m, MonadThrow m) => Text -> a -> m Text
    hashKey path _ = do
      hash <- createHash $ unpack path
      return $ (pack . show) hash

createHash :: (MonadThrow m, MonadIO m) => FilePath -> m (Digest SHA1)
createHash path = do
  files <- (liftIO . doesDirectoryExist) path >>= \case
    True -> do
      directoryFiles <- (fmap . fmap) (\x -> [path, x]) (liftIO . listDirectory $ path)
      return $ fmap joinPath directoryFiles
    False -> (liftIO . doesFileExist) path >>= \case
      True -> return [path]
      False -> throwM $ FileOrFolderDoesNotExist (pack path)
  byteStringFiles <- traverse (liftIO . BS.readFile) files
  let context = hashUpdates (hashInitWith SHA1) byteStringFiles
  return $ hashFinalize context
