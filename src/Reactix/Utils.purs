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

-- I'm not quite sure of the type of this. Purescript "helpfully" says it's:
onemore :: forall t24 t25 t27 t28. (t25 -> t27 -> t28) -> (t24 -> t25) -> (t24 -> t27) -> t24 -> t28
onemore s f g a = s (f a) (g a)

-- Aka the `s` combinator from the SKI calculus
splay1 :: forall a b c. (a -> b -> c) -> (a -> b) -> a -> c
splay1 c f v = c v (f v)

splay2 :: forall a b c d. (a -> b -> c -> d) -> (a -> b -> c) -> a -> b -> d
splay2 = onemore splay1

splay3 :: forall a b c d e. (a -> b -> c -> d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
splay3 = onemore splay2

splay4 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
splay4 = onemore splay3

splay5 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
splay5 = onemore splay4

