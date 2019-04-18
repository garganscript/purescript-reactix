module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (runMocha)
import Reactix.React.Spec
import Test.Spec.Runner (run)

import Reactix.React.Spec as ReactSpec

specs :: Spec Unit
specs = do ReactSpec.spec

main :: Effect Unit
main = do --  >>= run [consoleReporter]
  -- specs <- discoverSpecs
  runMocha specs
