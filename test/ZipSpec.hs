module ZipSpec (spec, main) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (pack)
import Network.AWS.S3.Types (BucketName(..))
import System.FilePath ( (</>) )
import Test.Hspec (describe , context , it , hspec , Spec)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)

import StackParameters (BucketFiles(..))
import Types (FileOrFolderDoesNotExist(..))
import Zip (inlineZips, writeZip, getPathInArchive)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ZipSpec" $ do
  context "replacing file paths and alternate file paths with zipped extensions" $ do
    it "can append paths and alternate paths with '.zip'" $ do
      let bucketFiles = BucketFiles
            { _bucketName = BucketName "SomeBucket"
            , _isHashed   = False
            , _isZipped   = True
            , _paths      = [
                             ("file1.txt", "newName")
                            ,(pack $ "path" </> "to" </> "file.txt", pack $ "path" </> "to" </> "file.txt")
                            ,("folder", "folder")
                            ,(pack $ "path" </> "to" </> "folder2", pack $ "path" </> "to" </> "folder2")
                            ]
            }
      let output = BucketFiles
            { _bucketName = BucketName "SomeBucket"
            , _isHashed   = False
            , _isZipped   = True
            , _paths      = [
                             ("file1.txt.zip", "newName.zip")
                            ,("file.txt.zip", pack $ "path" </> "to" </> "file.txt.zip")
                            ,("folder.zip", "folder.zip")
                            ,("folder2.zip", pack $ "path" </> "to" </> "folder2.zip")
                            ]
            }
      inlineZips bucketFiles `shouldBe` output

    it "should throw if a file or folder can't be found" $ do
      let path = pack $ "this" </> "file" </> "does" </> "not" </> "exit.blah"
      (runResourceT . writeZip) path `shouldThrow` (== FileOrFolderDoesNotExist path)

  context "archive paths" $ do
    it "file in empty root path" $ do
      let rootPath = ""
      let path = "file.txt"
      getPathInArchive rootPath path `shouldBe` ""

    it "file in folder in empty root path" $ do
      let rootPath = ""
      let path = "folder" </> "file.txt"
      getPathInArchive rootPath path `shouldBe` "folder"

    it "file in folder root path" $ do
      let rootPath = "folder"
      let path = "folder" </> "file.txt"
      getPathInArchive rootPath path `shouldBe` ""

    it "file in folder in folder root path" $ do
      let rootPath = "folder"
      let path = "folder" </> "folder2" </> "file.txt"
      getPathInArchive rootPath path `shouldBe` "folder2"
