module HashSpec (spec, main) where

import Network.AWS.S3.Types (BucketName(..))
import Test.Hspec (describe, context, it, hspec, Spec)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)

import Hash ( inlineHashes
            , createHash
            , HashNotFound(..)
            , FileOrFolderDoesNotExist(..)
            )
import StackParameters (BucketFiles(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HashSpec" $ do
  context "replacing file paths with hashed file paths" $ do
    it "can prepend alternate paths with hashes" $ do
      let fileHashes = [("file1.txt", "abcd1234wxyz")]
      let bucketFiles = BucketFiles
            { _bucketName = BucketName "${stack.some-stack.output.BucketName}"
            , _isHashed   = True
            , _isZipped   = False
            , _paths      = [("file1.txt", "folder/file1.txt")]
            }
      let output = BucketFiles
            { _bucketName = BucketName "${stack.some-stack.output.BucketName}"
            , _isHashed   = True
            , _isZipped   = False
            , _paths      = [("file1.txt", "abcd1234wxyz/folder/file1.txt")]
            }
      hashedBucketFiles <- inlineHashes fileHashes bucketFiles
      hashedBucketFiles `shouldBe` output

    it "doesn't alter bucketfiles that are not hashed" $ do
      let fileHashes = [("file1.txt", "abcd1234wxyz")]
      let bucketFiles = BucketFiles
            { _bucketName = BucketName "${stack.some-stack.output.BucketName}"
            , _isHashed   = False
            , _isZipped   = False
            , _paths      = [("file1.txt", "folder/file1.txt")]
            }
      let output = BucketFiles
            { _bucketName = BucketName "${stack.some-stack.output.BucketName}"
            , _isHashed   = False
            , _isZipped   = False
            , _paths      = [("file1.txt", "folder/file1.txt")]
            }
      unhashedBucketFiles <- inlineHashes fileHashes bucketFiles
      unhashedBucketFiles `shouldBe` output

    it "throws if a hash cannot be found" $ do
      let fileHashes = [("file1.txt", "abcd1234wxyz")]
      let bucketFiles = BucketFiles
            { _bucketName = BucketName "${stack.some-stack.output.BucketName}"
            , _isHashed   = True
            , _isZipped   = False
            , _paths      = [("file100.txt", "folder/file100.txt")]
            }
      inlineHashes fileHashes bucketFiles `shouldThrow`
        (== HashNotFound "file100.txt")

  context "hashing files" $
    it "throws if the file or folder doesn't exist" $
      createHash "file1.txt" `shouldThrow`
        (== FileOrFolderDoesNotExist "file1.txt")

    it "can hash a single file" $
      void $ createHash "SOMEPATH"

    it "can the contents" $ undefined
