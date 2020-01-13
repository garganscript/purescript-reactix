module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (Spec)
import Test.Spec.Mocha (runMocha)

import Reactix.React.Spec as ReactSpec

specs :: Spec Unit
specs = do ReactSpec.spec

main :: Effect Unit
main = do
  runMocha $ pure specs
