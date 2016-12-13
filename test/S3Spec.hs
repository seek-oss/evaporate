module S3Spec where

import Test.Hspec (describe, context, it, hspec, Spec)
import Test.Hspec.Expectations.Pretty (shouldBe)

import S3 (makeS3FilePath)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "S3" $ do
  context "making alternate paths for folders" $ do
    it "can make an alternate path for a non-nested folder, a non-nested file \
       \and a non-nested alternate folder path" $ do
      let folderPath = "folder1"
      let filePath = "folder1/test.txt"
      let altFolderPath = "myFolder"
      let result = makeS3FilePath altFolderPath folderPath filePath
      result `shouldBe` "myFolder/test.txt"

    it "can make an alternate path for a non-nested folder, a nested file and \
       \a non-nested alternate folder path" $ do
      let folderPath = "folder1"
      let filePath = "folder1/folder2/test.txt"
      let altFolderPath = "myFolder"
      let result = makeS3FilePath altFolderPath folderPath filePath
      result `shouldBe` "myFolder/folder2/test.txt"

    it "can make an alternate path for a nested folder, a nested file and a \
       \non-nested alternate folder path" $ do
      let folderPath = "folder1/folder2"
      let filePath = "folder1/folder2/test.txt"
      let altFolderPath = "myFolder"
      let result = makeS3FilePath altFolderPath folderPath filePath
      result `shouldBe` "myFolder/test.txt"

    it "can make an alternate path for a non-nested folder, a non-nested file \
       \and a nested alternate folder path" $ do
      let folderPath = "folder1"
      let filePath = "folder1/test.txt"
      let altFolderPath = "myFolder/myInnerFolder"
      let result = makeS3FilePath altFolderPath folderPath filePath
      result `shouldBe` "myFolder/myInnerFolder/test.txt"

    it "can make an alternate path for a nested folder, a nested file and a \
       \nested alternate folder path" $ do
      let folderPath = "folder1/folder2"
      let filePath = "folder1/folder2/test.txt"
      let altFolderPath = "myFolder/myInnerFolder"
      let result = makeS3FilePath altFolderPath folderPath filePath
      result `shouldBe` "myFolder/myInnerFolder/test.txt"
