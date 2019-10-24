module Test.Main where

import Prelude (Unit, identity, pure)
import Data.Identity (Identity(..))
import Effect (Effect)
import Test.Spec (Spec, mapSpecTree)
import Test.Spec.Mocha (runMocha)

import Reactix.React.Spec as ReactSpec

specs :: Spec Unit
specs = ReactSpec.spec

main :: Effect Unit
main = runMocha (mapSpecTree (\(Identity a) -> pure a) identity specs)
