module Reactix.Hooks
  ( useState, State
  , useEffect, useEffect1, useEffect2, useEffect3, useEffect4, useEffect5
  , useLayoutEffect, useLayoutEffect1, useLayoutEffect2
  , useLayoutEffect3, useLayoutEffect4, useLayoutEffect5
  , useMemo, useMemo1, useMemo2, useMemo3, useMemo4, useMemo5
  , useReducer, useReducer', Reducer
  , useRef, Ref                                                   
  , useContext
  , useDebugValue, useDebugValue'
  -- , useImperativeHandle
  )
 where

import Prelude
import Data.Function.Uncurried (Fn2, mkFn2, runFn2, Fn3, runFn3, Fn4, runFn4, Fn5, runFn5, Fn6, runFn6)
import Effect (Effect)
import Effect.Uncurried ( EffectFn2, runEffectFn2 )

import Reactix.React ( class MonadHooks, Context )



-- | A state hook
foreign import data State :: Type -> Type

-- | A ref hook
foreign import data Ref :: Type -> Type

-- | A reducer hook
foreign import data Reducer :: Type -> Type -> Type

-- | Takes an effect function which calculates the initial state
foreign import _useState :: forall s. Effect s -> State s

-- | Given an Effect function returning an initial value, returns a UseState hook
useState :: forall s m. MonadHooks m => Effect s -> m (State s)
useState = pure <<< _useState

-- instance readState :: Read (State s) s where
--   read = _read0

-- instance writeState :: Write (State s) s where
--   write = runEffectFn2 _call1

foreign import _read0 :: forall a b. a -> b

foreign import _call1 :: forall a b c. EffectFn2 a b c


-- useEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called after rendering
useEffect :: forall m. MonadHooks m => Effect (Effect Unit) -> m Unit
useEffect = pure <<< _useEffect

-- | Like useEffect, but with a memo value
useEffect1 :: forall a m. MonadHooks m => a -> Effect (Effect Unit) -> m Unit
useEffect1 a = pure <<< runFn2 _useEffect1 a

-- | Like useEffect, but with 2 memo values
useEffect2 :: forall a b m. MonadHooks m => a -> b -> Effect (Effect Unit) -> m Unit
useEffect2 a b = pure <<< runFn3 _useEffect2 a b

-- | Like useEffect, but with 3 memo values
useEffect3 :: forall a b c m. MonadHooks m => a -> b -> c -> Effect (Effect Unit) -> m Unit
useEffect3 a b c = pure <<< runFn4 _useEffect3 a b c

-- | Like useEffect, but with 4 memo values
useEffect4 :: forall a b c d m. MonadHooks m => a -> b -> c -> d -> Effect (Effect Unit) -> m Unit
useEffect4 a b c d = pure <<< runFn5 _useEffect4 a b c d

-- | Like useEffect, but with 5 memo values
useEffect5 :: forall a b c d e m. MonadHooks m => a -> b -> c -> d -> e -> Effect (Effect Unit) -> m Unit
useEffect5 a b c d e = pure <<< runFn6 _useEffect5 a b c d e

foreign import _useEffect :: Effect (Effect Unit) -> Unit
foreign import _useEffect1 :: forall a. Fn2 a (Effect (Effect Unit)) Unit
foreign import _useEffect2 :: forall a b. Fn3 a b (Effect (Effect Unit)) Unit
foreign import _useEffect3 :: forall a b c. Fn4 a b c(Effect (Effect Unit)) Unit
foreign import _useEffect4 :: forall a b c d. Fn5 a b c d (Effect (Effect Unit)) Unit
foreign import _useEffect5 :: forall a b c d e. Fn6 a b c d e (Effect (Effect Unit)) Unit





-- useLayoutEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called in the same phase as
-- | `componentDidMount` and `componentDidUpdate` used to be.
useLayoutEffect :: forall m. MonadHooks m => Effect (Effect Unit) -> m Unit
useLayoutEffect = pure <<< _useLayoutEffect

-- | Like useLayoutEffect, but with a memo value
useLayoutEffect1 :: forall a m. MonadHooks m => a -> Effect (Effect Unit) -> m Unit
useLayoutEffect1 a = pure <<< runFn2 _useLayoutEffect1 a

-- | Like useLayoutEffect, but with 2 memo values
useLayoutEffect2 :: forall a b m. MonadHooks m => a -> b -> Effect (Effect Unit) -> m Unit
useLayoutEffect2 a b = pure <<< runFn3 _useLayoutEffect2 a b

-- | Like useLayoutEffect, but with 3 memo values
useLayoutEffect3 :: forall a b c m. MonadHooks m => a -> b -> c -> Effect (Effect Unit) -> m Unit
useLayoutEffect3 a b c = pure <<< runFn4 _useLayoutEffect3 a b c

-- | Like useLayoutEffect, but with 4 memo values
useLayoutEffect4 :: forall a b c d m. MonadHooks m => a -> b -> c -> d -> Effect (Effect Unit) -> m Unit
useLayoutEffect4 a b c d = pure <<< runFn5 _useLayoutEffect4 a b c d

-- | Like useLayoutEffect, but with 5 memo values
useLayoutEffect5 :: forall a b c d e m. MonadHooks m => a -> b -> c -> d -> e -> Effect (Effect Unit) -> m Unit
useLayoutEffect5 a b c d e= pure <<< runFn6 _useLayoutEffect5 a b c d e

foreign import _useLayoutEffect :: Effect (Effect Unit) -> Unit
foreign import _useLayoutEffect1 :: forall a. Fn2 a (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect2 :: forall a b. Fn3 a b (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect3 :: forall a b c. Fn4 a b c(Effect (Effect Unit)) Unit
foreign import _useLayoutEffect4 :: forall a b c d. Fn5 a b c d (Effect (Effect Unit)) Unit
foreign import _useLayoutEffect5 :: forall a b c d e. Fn6 a b c d e (Effect (Effect Unit)) Unit





-- useMemo


useMemo :: forall a m. MonadHooks m => Effect a -> m a
useMemo = pure <<< _useMemo
useMemo1 :: forall a b m. MonadHooks m => b -> Effect a -> m a
useMemo1 a = pure <<< runFn2 _useMemo1 a
useMemo2 :: forall a b c m. MonadHooks m => b -> c -> Effect a -> m a
useMemo2 a b = pure <<< runFn3 _useMemo2 a b
useMemo3 :: forall a b c d m. MonadHooks m => b -> c -> d -> Effect a -> m a
useMemo3 a b c = pure <<< runFn4 _useMemo3 a b c
useMemo4 :: forall a b c d e m. MonadHooks m => b -> c -> d -> e -> Effect a -> m a
useMemo4 a b c d = pure <<< runFn5 _useMemo4 a b c d
useMemo5 :: forall a b c d e f m. MonadHooks m => b -> c -> d -> e -> f -> Effect a -> m a
useMemo5 a b c d e = pure <<< runFn6 _useMemo5 a b c d e

foreign import _useMemo :: forall a. Effect a -> a
foreign import _useMemo1 :: forall a b. Fn2 b (Effect a) a
foreign import _useMemo2 :: forall a b c. Fn3 b c (Effect a) a
foreign import _useMemo3 :: forall a b c d. Fn4 b c d (Effect a) a
foreign import _useMemo4 :: forall a b c d e. Fn5 b c d e (Effect a) a
foreign import _useMemo5 :: forall a b c d e f. Fn6 b c d e f (Effect a) a




-- useRef

foreign import _useRef :: forall r. r -> Ref r

useRef :: forall r m. MonadHooks m => r -> m (Ref r)
useRef = pure <<< _useRef

-- instance readRef :: Read (Ref r) r where
--   read = _readCurrent

-- instance writeRef :: Write (Ref r) r where
--   write = runEffectFn2 _writeCurrent

foreign import _readCurrent :: forall a b. a -> b
foreign import _writeCurrent :: forall a b c. EffectFn2 a b c


-- useReducer

useReducer :: forall s a i m. MonadHooks m => (s -> a -> s) -> (i -> s) -> i -> m (Reducer s a)
useReducer f i j = pure $ runFn3 _useReducer (mkFn2 f) j i

useReducer' :: forall s a m. MonadHooks m => (s -> a -> s) -> s -> m (Reducer s a)
useReducer' r = useReducer r identity

-- instance readReducer :: Read (Reducer s a) s where
--   read = _read0

-- instance writeReducer :: Write (Reducer s a) a where
--   write = runEffectFn2 _call1

foreign import _useReducer :: forall s a i. Fn3 (Fn2 s a s) i (i -> s) (Reducer s a)

-- useContext

foreign import _useContext :: forall a m. MonadHooks m => Context a -> m a

useContext :: forall a m. MonadHooks m => Context a -> m a
useContext = _useContext




-- useDebugValue

useDebugValue :: forall v v' m. MonadHooks m => v -> (v -> v') -> m Unit
useDebugValue v f = pure $ runFn2 _useDebugValue v f

useDebugValue' :: forall v m. MonadHooks m => v -> m Unit
useDebugValue' = pure <<< _useDebugValuePrime

foreign import _useDebugValue :: forall v v'. Fn2 v (v -> v') Unit
foreign import _useDebugValuePrime :: forall v. v -> Unit


-- foreign import _useImperativeHandle ::
