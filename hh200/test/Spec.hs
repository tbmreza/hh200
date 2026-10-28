import           Test.Tasty
import           Test.Tasty.Runners (NumThreads (..))

import qualified LspSpec
import qualified ScannerSpec
import qualified ExecutionSpec
import qualified MustacheSpec
import qualified DashboardSpec
import qualified TokenBucketWorkerPoolSpec

-- Run everything on a single thread: the Dashboard tests mutate process-global
-- environment variables (HH200_SQLITE, HH200_CONTROL_SOCKET), so they must not
-- run concurrently with one another.
main :: IO ()
main = defaultMain $ localOption (NumThreads 1) $ testGroup "Hh200 Tests"
  [ LspSpec.spec
  , ScannerSpec.spec
  , MustacheSpec.spec
  , ExecutionSpec.spec
  , DashboardSpec.spec
  , TokenBucketWorkerPoolSpec.spec
  ]
