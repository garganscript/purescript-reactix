module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Reactix.React.Spec as ReactSpec
import Test.Spec (SpecT)
import Test.Spec.Mocha (runMocha)

specs :: SpecT Aff Unit Effect Unit
specs = do ReactSpec.spec

main :: Effect Unit
main = do
  runMocha specs
