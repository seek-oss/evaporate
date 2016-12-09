module S3Spec where

import Test.Hspec (describe, context, it, hspec, Spec)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldThrow)

import S3 (getFiles, uploadFileOrFolder, replaceRootDirectoryWithAltPath)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "S3" $ do
  -- context "uploading a file to s3" $
  --   it "should throw if the file doesn't exist"
  --     let
  context "replacing folder names" $ do
    it "should replace the root folder for relative paths" $ do
      let path = "../data"
    it "should replace the root folder for absolute paths" $ do
