module Reactix.Hooks
  ( State, useState
  , HookEffect
  , useEffect, useEffect1, useEffect2, useEffect3, useEffect4, useEffect5
  , useLayoutEffect, useLayoutEffect1, useLayoutEffect2
  , useLayoutEffect3, useLayoutEffect4, useLayoutEffect5
  -- , Reducer
  -- , useReducer, useReducer'
  -- , useContext
  -- , useMemo, useMemo1, useMemo2 --, useMemo3, useMemo4, useMemo5
  , useRef
  -- , useDebugValue, useDebugValue'
  -- , useImperativeHandle
  )
 where

import Prelude
import Data.Function.Uncurried ( Fn2, mkFn2, runFn2 )
import Data.Tuple ( Tuple(..) )
import Effect ( Effect )
import Effect.Uncurried ( EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4, EffectFn5, runEffectFn5, EffectFn6, runEffectFn6 )
import FFI.Simple ( (...), (..), delay, args2, args3, args4, args5, setProperty )
import DOM.Simple.Console 

import Reactix.React ( Ref, Hooks, react, unsafeHooksEffect )

--- useState

-- | A state hook is a tuple of value and setter
type State state = Tuple state (state -> Effect Unit)

-- | Given an Effect function returning an initial value, returns a State
useState :: forall s. (Unit -> Effect s) -> Hooks (State s)
useState s = hook $ \_ -> pure $ currySecond $ tuple $ react ... "useState" $ [ delay unit s ]
-- -- useReducer

-- type Reducer state action = Tuple state (EffectFn1 action Unit)

-- useReducer :: forall s a i. (s -> a -> s) -> (i -> s) -> i -> Hooks (Reducer s a)
-- useReducer f i j = pure $ tuple $ react ... "useReducer" $ args3 f i j

-- useReducer' :: forall s a. (s -> a -> s) -> s -> Hooks (Reducer s a)
-- useReducer' r = useReducer r identity

-- instance readReducer :: Read (Reducer s a) s where
--   read = _read0

-- instance writeReducer :: Write (Reducer s a) a where
--   write = runEffectFn2 _call1




-- useEffect


type HookEffect = Unit -> Effect (Unit -> Effect Unit)

wrapEffect :: HookEffect -> Effect (Effect Unit)
wrapEffect f = delay unit $ \_ -> do
  cleanup <- f unit
  pure $ delay unit cleanup

_useEffect :: forall a. HookEffect -> a -> Hooks Unit
_useEffect e a = hook $ \_ -> pure $ react ... "useEffect" $ args2 (wrapEffect e) a

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called after rendering
useEffect :: HookEffect -> Hooks Unit
useEffect e = hook $ \_ -> pure $ react ... "useEffect" $ [ wrapEffect e ]

-- | Like useEffect, but with a memo value
useEffect1 :: forall a. a -> HookEffect -> Hooks Unit
useEffect1 a e = _useEffect e [a]

-- | Like useEffect, but with 2 memo values
useEffect2 :: forall a b. a -> b -> HookEffect -> Hooks Unit
useEffect2 a b e = _useEffect e $ args2 a b

-- | Like useEffect, but with 3 memo values
useEffect3 :: forall a b c. a -> b -> c -> HookEffect -> Hooks Unit
useEffect3 a b c e = _useEffect e $ args3 a b c

-- | Like useEffect, but with 4 memo values
useEffect4 :: forall a b c d. a -> b -> c -> d -> HookEffect -> Hooks Unit
useEffect4 a b c d e = _useEffect e $ args4 a b c d

-- | Like useEffect, but with 5 memo values
useEffect5 :: forall a b c d e. a -> b -> c -> d -> e -> HookEffect -> Hooks Unit
useEffect5 a b c d f e = _useEffect e $ args5 a b c d f

-- useLayoutEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called in the same phase as
-- | `componentDidMount` and `componentDidUpdate` used to be.
useLayoutEffect :: HookEffect -> Hooks Unit
useLayoutEffect e = hook $ \_ -> pure $ react ... "useLayoutEffect" $ [ wrapEffect e ]

_useLayoutEffect :: forall a. HookEffect -> a -> Hooks Unit
_useLayoutEffect e a = hook $ \_ -> pure $ react ... "useLayoutEffect" $ args2 (wrapEffect e) a

-- | Like useLayoutEffect, but with a memo value
useLayoutEffect1 :: forall a. a -> HookEffect -> Hooks Unit
useLayoutEffect1 a e = _useLayoutEffect e [a]

-- | Like useLayoutEffect, but with 2 memo values
useLayoutEffect2 :: forall a b. a -> b -> HookEffect -> Hooks Unit
useLayoutEffect2 a b e = _useLayoutEffect e $ args2 a b

-- | Like useLayoutEffect, but with 3 memo values
useLayoutEffect3 :: forall a b c. a -> b -> c -> HookEffect -> Hooks Unit
useLayoutEffect3 a b c e = _useLayoutEffect e $ args3 a b c

-- | Like useLayoutEffect, but with 4 memo values
useLayoutEffect4 :: forall a b c d. a -> b -> c -> d -> HookEffect -> Hooks Unit
useLayoutEffect4 a b c d e = _useLayoutEffect e $ args4 a b c d

-- | Like useLayoutEffect, but with 5 memo values
useLayoutEffect5 :: forall a b c d e. a -> b -> c -> d -> e -> HookEffect -> Hooks Unit
useLayoutEffect5 a b c d f e = _useLayoutEffect e $ args5 a b c d f

-- useMemo


-- useMemo :: forall a. Effect a -> Hooks a
-- useMemo = unsafeHooksEffect <<< runEffectFn1 _useMemo
-- useMemo1 :: forall a b. b -> Effect a -> Hooks a
-- useMemo1 a = unsafeHooksEffect <<< runEffectFn2 _useMemo1 a
-- useMemo2 :: forall a b c. b -> c -> Effect a -> Hooks a
-- useMemo2 a b = unsafeHooksEffect <<< runEffectFn3 _useMemo2 a b

-- foreign import _useMemo :: forall a. EffectFn1 (Effect a) a
-- foreign import _useMemo1 :: forall a b. EffectFn2 b (Effect a) a
-- foreign import _useMemo2 :: forall a b c. EffectFn3 b c (Effect a) a




-- useRef

useRef :: forall r. r -> Hooks (Ref r)
useRef r = hook $ \_ -> pure $ react ... "useRef" $ [ r ]

-- useContext

-- foreign import _useContext :: forall a. Context a -> Hooks a

-- useContext :: forall a. Context a -> Hooks a
-- useContext = _useContext




-- useDebugValue

useDebugValue :: forall v v'. v -> (v -> v') -> Hooks Unit
useDebugValue v = unsafeHooksEffect <<< runEffectFn2 _useDebugValue v

useDebugValue' :: forall v. v -> Hooks Unit
useDebugValue' = unsafeHooksEffect <<< runEffectFn1 _useDebugValuePrime

foreign import _useDebugValue :: forall v v'. EffectFn2 v (v -> v') Unit
foreign import _useDebugValuePrime :: forall v. EffectFn1 v Unit
-- foreign import _useImperativeHandle ::


-- ffi utilities


tuple :: forall a b c. a -> Tuple b c
tuple = runFn2 _tuple Tuple

foreign import _tuple :: forall a b c. Fn2 (a -> b -> Tuple a b) c (Tuple a b)

tupleCurrent :: forall a b c. a -> Tuple b c
tupleCurrent = runFn2 _tupleCurrent Tuple

foreign import _tupleCurrent :: forall a b c. Fn2 (a -> b -> Tuple a b) c (Tuple a b)

currySecond :: forall a b c. Tuple a (EffectFn1 b c) -> Tuple a (b -> Effect c)
currySecond (Tuple a b) = Tuple a (runEffectFn1 b)

hook :: forall v. (Unit -> Effect v) -> Hooks v
hook f = unsafeHooksEffect (delay unit f)


