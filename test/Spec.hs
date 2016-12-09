import Test.Hspec

import qualified ExternalValuesSpec
import qualified HashSpec
import qualified LoggingSpec
import qualified S3Spec
import qualified StackDependencySpec
import qualified StackParametersSpec
import qualified ZipSpec

main :: IO ()
main = do
  putStrLn "\nStarting tests..."
  hspec spec

spec :: Spec
spec = do
  ExternalValuesSpec.spec
  HashSpec.spec
  LoggingSpec.spec
  S3Spec.spec
  StackDependencySpec.spec
  StackParametersSpec.spec
  ZipSpec.spec
