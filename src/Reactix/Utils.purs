module Reactix.Utils where

import Prelude ((<<<), (<>))
import Data.Unit (Unit, unit)
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.String (splitAt, toUpper)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import FFI.Simple (delay)
import Reactix.React (Hooks, unsafeHooksEffect)

tuple :: forall a b c. a -> Tuple b c
tuple = runFn2 _tuple Tuple

foreign import _tuple :: forall a b c. Fn2 (a -> b -> Tuple a b) c (Tuple a b)

currySecond :: forall a b c. Tuple a (EffectFn1 b c) -> Tuple a (b -> Effect c)
currySecond (Tuple a b) = Tuple a (runEffectFn1 b)

hook :: forall v. (Unit -> Effect v) -> Hooks v
hook f = unsafeHooksEffect (delay unit f)

ucFirst :: String -> String
ucFirst = help <<< splitAt 1
  where help {before, after} = toUpper before <> after
