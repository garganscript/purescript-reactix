module Reactix.Hooks
  ( State, useState
  , useEffect, useEffect1, useEffect2, useEffect3, useEffect4, useEffect5
  , useLayoutEffect, useLayoutEffect1, useLayoutEffect2
  , useLayoutEffect3, useLayoutEffect4, useLayoutEffect5
  , useMemo, useMemo1, useMemo2, useMemo3, useMemo4, useMemo5
  , Reducer, useReducer, useReducer'
  , Ref, useRef
  , useContext
  , useDebugValue, useDebugValue'
  -- , useImperativeHandle
  )
 where

import Prelude
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried ( EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4, EffectFn5, runEffectFn5, EffectFn6, runEffectFn6 )

import Reactix.React ( class MonadHooks, unsafeHooksEffect, Context )


--- useState

-- | A state hook is a tuple of value and setter
type State state = Tuple state (EffectFn1 state Unit)

-- | Given an Effect function returning an initial value, returns a UseState hook
useState :: forall s m. MonadHooks m => Effect s -> m (State s)
useState = unsafeHooksEffect <<< runEffectFn2 _useState Tuple

-- | Takes an effect function which calculates the initial state
foreign import _useState :: forall s t. EffectFn2 (s -> t -> Tuple s t) (Effect s) (State s)


-- useEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called after rendering
useEffect :: forall m. MonadHooks m => Effect (Effect Unit) -> m Unit
useEffect = unsafeHooksEffect <<< runEffectFn1 _useEffect

-- | Like useEffect, but with a memo value
useEffect1 :: forall a m. MonadHooks m => a -> Effect (Effect Unit) -> m Unit
useEffect1 a = unsafeHooksEffect <<< runEffectFn2 _useEffect1 a

-- | Like useEffect, but with 2 memo values
useEffect2 :: forall a b m. MonadHooks m => a -> b -> Effect (Effect Unit) -> m Unit
useEffect2 a b = unsafeHooksEffect <<< runEffectFn3 _useEffect2 a b

-- | Like useEffect, but with 3 memo values
useEffect3 :: forall a b c m. MonadHooks m => a -> b -> c -> Effect (Effect Unit) -> m Unit
useEffect3 a b c = unsafeHooksEffect <<< runEffectFn4 _useEffect3 a b c

-- | Like useEffect, but with 4 memo values
useEffect4 :: forall a b c d m. MonadHooks m => a -> b -> c -> d -> Effect (Effect Unit) -> m Unit
useEffect4 a b c d = unsafeHooksEffect <<< runEffectFn5 _useEffect4 a b c d

-- | Like useEffect, but with 5 memo values
useEffect5 :: forall a b c d e m. MonadHooks m => a -> b -> c -> d -> e -> Effect (Effect Unit) -> m Unit
useEffect5 a b c d e = unsafeHooksEffect <<< runEffectFn6 _useEffect5 a b c d e

foreign import _useEffect :: EffectFn1 (Effect (Effect Unit)) Unit
foreign import _useEffect1 :: forall a. EffectFn2 a (Effect (Effect Unit)) Unit
foreign import _useEffect2 :: forall a b. EffectFn3 a b (Effect (Effect Unit)) Unit
foreign import _useEffect3 :: forall a b c. EffectFn4 a b c(Effect (Effect Unit)) Unit
foreign import _useEffect4 :: forall a b c d. EffectFn5 a b c d (Effect (Effect Unit)) Unit
foreign import _useEffect5 :: forall a b c d e. EffectFn6 a b c d e (Effect (Effect Unit)) Unit





-- useLayoutEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called in the same phase as
-- | `componentDidMount` and `componentDidUpdate` used to be.
useLayoutEffect :: forall m. MonadHooks m => Effect (Effect Unit) -> m Unit
useLayoutEffect = unsafeHooksEffect <<< runEffectFn1 _useLayoutEffect

-- | Like useLayoutEffect, but with a memo value
useLayoutEffect1 :: forall a m. MonadHooks m => a -> Effect (Effect Unit) -> m Unit
useLayoutEffect1 a = unsafeHooksEffect <<< runEffectFn2 _useLayoutEffect1 a

-- | Like useLayoutEffect, but with 2 memo values
useLayoutEffect2 :: forall a b m. MonadHooks m => a -> b -> Effect (Effect Unit) -> m Unit
useLayoutEffect2 a b = unsafeHooksEffect <<< runEffectFn3 _useLayoutEffect2 a b

-- | Like useLayoutEffect, but with 3 memo values
useLayoutEffect3 :: forall a b c m. MonadHooks m => a -> b -> c -> Effect (Effect Unit) -> m Unit
useLayoutEffect3 a b c = unsafeHooksEffect <<< runEffectFn4 _useLayoutEffect3 a b c

-- | Like useLayoutEffect, but with 4 memo values
useLayoutEffect4 :: forall a b c d m. MonadHooks m => a -> b -> c -> d -> Effect (Effect Unit) -> m Unit
useLayoutEffect4 a b c d = unsafeHooksEffect <<< runEffectFn5 _useLayoutEffect4 a b c d

-- | Like useLayoutEffect, but with 5 memo values
useLayoutEffect5 :: forall a b c d e m. MonadHooks m => a -> b -> c -> d -> e -> Effect (Effect Unit) -> m Unit
useLayoutEffect5 a b c d e= unsafeHooksEffect <<< runEffectFn6 _useLayoutEffect5 a b c d e

foreign import _useLayoutEffect :: EffectFn1 (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect1 :: forall a. EffectFn2 a (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect2 :: forall a b. EffectFn3 a b (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect3 :: forall a b c. EffectFn4 a b c(Effect (Effect Unit)) Unit
foreign import _useLayoutEffect4 :: forall a b c d. EffectFn5 a b c d (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect5 :: forall a b c d e. EffectFn6 a b c d e (Effect (Effect Unit)) Unit





-- useMemo


useMemo :: forall a m. MonadHooks m => Effect a -> m a
useMemo = unsafeHooksEffect <<< runEffectFn1 _useMemo
useMemo1 :: forall a b m. MonadHooks m => b -> Effect a -> m a
useMemo1 a = unsafeHooksEffect <<< runEffectFn2 _useMemo1 a
useMemo2 :: forall a b c m. MonadHooks m => b -> c -> Effect a -> m a
useMemo2 a b = unsafeHooksEffect <<< runEffectFn3 _useMemo2 a b
useMemo3 :: forall a b c d m. MonadHooks m => b -> c -> d -> Effect a -> m a
useMemo3 a b c = unsafeHooksEffect <<< runEffectFn4 _useMemo3 a b c
useMemo4 :: forall a b c d e m. MonadHooks m => b -> c -> d -> e -> Effect a -> m a
useMemo4 a b c d = unsafeHooksEffect <<< runEffectFn5 _useMemo4 a b c d
useMemo5 :: forall a b c d e f m. MonadHooks m => b -> c -> d -> e -> f -> Effect a -> m a
useMemo5 a b c d e = unsafeHooksEffect <<< runEffectFn6 _useMemo5 a b c d e

foreign import _useMemo :: forall a. EffectFn1 (Effect a) a
foreign import _useMemo1 :: forall a b. EffectFn2 b (Effect a) a
foreign import _useMemo2 :: forall a b c. EffectFn3 b c (Effect a) a
foreign import _useMemo3 :: forall a b c d. EffectFn4 b c d (Effect a) a
foreign import _useMemo4 :: forall a b c d e. EffectFn5 b c d e (Effect a) a
foreign import _useMemo5 :: forall a b c d e f. EffectFn6 b c d e f (Effect a) a




-- useRef

type Ref state = Tuple state (EffectFn1 state Unit)

foreign import _useRef :: forall r s. EffectFn2 (r -> s -> Tuple r s) r (Ref r)

useRef :: forall r m. MonadHooks m => r -> m (Ref r)
useRef r = unsafeHooksEffect $ runEffectFn2 _useRef Tuple r

-- useReducer

type Reducer state action = Tuple state (EffectFn1 action Unit)

useReducer :: forall s a i m. MonadHooks m => (s -> a -> s) -> (i -> s) -> i -> m (Reducer s a)
useReducer f i j = unsafeHooksEffect $ runEffectFn4 _useReducer Tuple (mkFn2 f) j i

useReducer' :: forall s a m. MonadHooks m => (s -> a -> s) -> s -> m (Reducer s a)
useReducer' r = useReducer r identity

-- instance readReducer :: Read (Reducer s a) s where
--   read = _read0

-- instance writeReducer :: Write (Reducer s a) a where
--   write = runEffectFn2 _call1

foreign import _useReducer :: forall s a i x y. EffectFn4 (x -> y -> Tuple x y) (Fn2 s a s) i (i -> s) (Reducer s a)

-- useContext

foreign import _useContext :: forall a m. MonadHooks m => Context a -> m a

useContext :: forall a m. MonadHooks m => Context a -> m a
useContext = _useContext




-- useDebugValue

useDebugValue :: forall v v' m. MonadHooks m => v -> (v -> v') -> m Unit
useDebugValue v = unsafeHooksEffect <<< runEffectFn2 _useDebugValue v

useDebugValue' :: forall v m. MonadHooks m => v -> m Unit
useDebugValue' = unsafeHooksEffect <<< runEffectFn1 _useDebugValuePrime

foreign import _useDebugValue :: forall v v'. EffectFn2 v (v -> v') Unit
foreign import _useDebugValuePrime :: forall v. EffectFn1 v Unit


-- foreign import _useImperativeHandle ::
