{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import           Control.Exception.Safe (Exception(..))
import           Data.Hashable (Hashable)
import           Data.HashMap.Lazy (HashMap)
import           Data.Monoid ((<>))
import           Data.Text (unpack, Text)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

type StackOutputs      = HashMap StackOutputName Text
type LoadedParameters  = HashMap Text Text
type FileHashes        = HashMap Text Text
type Paths             = HashMap Text Text
type AWSAccountID      = Text
type Tags              = HashMap Text Text

data PathType = File FilePath | Directory FilePath
  deriving (Show, Eq)

newtype EvaporateException = EvaporateException Text
  deriving (Eq, Show, Typeable)

instance Exception EvaporateException

newtype OutputName = OutputName Text
  deriving (Ord, Show, Eq)

newtype FileOrFolderDoesNotExist =
  FileOrFolderDoesNotExist Text
  deriving (Eq, Show, Typeable)

instance Exception FileOrFolderDoesNotExist where
  displayException (FileOrFolderDoesNotExist f) = unpack $
    "File/folder '" <> f <> "' does not exist."

newtype EnvironmentNotFound = EnvironmentNotFound AWSAccountID
  deriving (Eq, Show, Typeable)

instance Exception EnvironmentNotFound where
  displayException (EnvironmentNotFound a) = unpack $
    "Could not find parameters for AWS Account '" <> a <> "'."

newtype StackName = StackName { getStackName :: Text }
  deriving (Show, Eq, Generic, Ord)

instance Hashable StackName

newtype StackOutputLoadFailed = StackOutputLoadFailed StackName
  deriving (Eq, Show, Typeable)

instance Exception StackOutputLoadFailed where
  displayException (StackOutputLoadFailed s) = unpack $
    "Failed to load outputs for stack '" <> getStackName s <> "'."

data StackOutputName = StackOutputName {
    _soStackName  :: StackName
  , _soOutputName :: Text
  } deriving (Show, Eq, Ord, Generic)

instance Hashable StackOutputName

newtype HashNotFound = HashNotFound Text
  deriving (Eq, Show, Typeable)

instance Exception HashNotFound where
  displayException (HashNotFound f) = unpack $
       "Could not find hash for file/folder '" <> f
    <> "'. You may need to add 'hash: true' to the 's3upload' section of the \
    \stack that is referencing this value."
