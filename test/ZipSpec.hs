module ZipSpec (spec, main) where

import Control.Monad.Trans.Resource (runResourceT)
import Network.AWS.S3.Types (BucketName(..))
import Test.Hspec (describe , context , it , hspec , Spec)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)

import StackParameters (BucketFiles(..))
import Types (FileOrFolderDoesNotExist(..))
import Zip(inlineZips, writeZip)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ZipSpec" $
  context "replacing file paths and alternate file paths with zipped extensions" $ do
    it "can append paths and alternate paths with '.zip'" $ do
      let bucketFiles = BucketFiles
            { _bucketName = BucketName "SomeBucket"
            , _isHashed   = False
            , _isZipped   = True
            , _paths      = [
                             ("file1.txt", "newName")
                            ,("path/to/file.txt", "path/to/file.txt")
                            ,("folder", "folder")
                            ,("path/to/folder2", "path/to/folder2")
                            ]
            }
      let output = BucketFiles
            { _bucketName = BucketName "SomeBucket"
            , _isHashed   = False
            , _isZipped   = True
            , _paths      = [
                             ("file1.txt.zip", "newName.zip")
                            ,("file.txt.zip", "path/to/file.txt.zip")
                            ,("folder.zip", "folder.zip")
                            ,("folder2.zip", "path/to/folder2.zip")
                            ]
            }
      inlineZips bucketFiles `shouldBe` output

    it "should throw if a file or folder can't be found" $ do
      let path = "this/file/does/not/exit.blah"
      (runResourceT . writeZip) path `shouldThrow` (== FileOrFolderDoesNotExist path)
