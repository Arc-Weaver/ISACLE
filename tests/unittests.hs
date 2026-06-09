import Prelude

import Test.Tasty

import qualified Tests.Core.Harvard.ISA
import qualified Tests.Core.Harvard.Pipeline
import qualified Tests.Core.GPIO

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Core.Harvard.ISA.isaTests
  , Tests.Core.Harvard.Pipeline.pipelineTests
  , Tests.Core.GPIO.gpioTests
  ]
