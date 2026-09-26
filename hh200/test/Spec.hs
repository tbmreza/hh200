import           Test.Tasty

import qualified LspSpec
import qualified ScannerSpec
import qualified ExecutionSpec
import qualified MustacheSpec

main :: IO ()
main = defaultMain $ testGroup "Hh200 Tests"
  [ LspSpec.spec
  , ScannerSpec.spec
  , MustacheSpec.spec
  , ExecutionSpec.spec
  ]
