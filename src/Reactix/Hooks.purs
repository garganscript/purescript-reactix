module Reactix.Hooks
  ( State, useState, useState'
  , Reducer, useReducer, useReducer'
  , useContext
  , useRef
  , useDebugValue, useDebugValue'
  , HookEffect, nothing, thenNothing
  , useEffect,  useEffect',  useEffect1, useEffect1'
  , useEffect2, useEffect2', useEffect3, useEffect3'
  , useEffect4, useEffect4', useEffect5, useEffect5'
  , useLayoutEffect,  useLayoutEffect'
  , useLayoutEffect1, useLayoutEffect1'
  , useLayoutEffect2, useLayoutEffect2'
  , useLayoutEffect3, useLayoutEffect3'
  , useLayoutEffect4, useLayoutEffect4'
  , useLayoutEffect5, useLayoutEffect5'
  , useMemo, useMemo1, useMemo2, useMemo3, useMemo4, useMemo5
  , useCallback,  useCallback1, useCallback2
  , useCallback3, useCallback4, useCallback5
  , useImperativeHandle,  useImperativeHandle1, useImperativeHandle2
  , useImperativeHandle3, useImperativeHandle4, useImperativeHandle5
  )
 where

import Prelude
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn1, runEffectFn1, mkEffectFn1
  , EffectFn2, runEffectFn2, EffectFn3, runEffectFn3
  , EffectFn4, runEffectFn4, EffectFn5, runEffectFn5, EffectFn6, runEffectFn6 )
import FFI.Simple ((...), (..), delay, args2, args3, args4, args5, setProperty)
import FFI.Simple.PseudoArray as Array
import DOM.Simple.Console 
import Reactix.Utils (tuple, currySecond, hook)
import Reactix.React (Context, Ref, Hooks, react)

--- useState

-- | A state hook is a tuple of value and setter
type State state = Tuple state (state -> Effect Unit)

-- | Given an Effect function returning an initial value, returns a State
useState :: forall s. (Unit -> Effect s) -> Hooks (State s)
useState s =
  hook $ \_ ->
    pure $ currySecond $ tuple $ react ... "useState" $ [ delay unit s ]

useState' :: forall s. s -> Hooks (State s)
useState' s = useState $ \_ -> pure s
-- useReducer

-- | A reducer hook is a tuple of value and reducer-setter
type Reducer state action = Tuple state (action -> Effect Unit)

-- | Given a reducer function from a state and action to a new state,
-- | an initialiser function and argument for the initialiser, returns
-- | a Reducer. Note args 2 and 3 are swapped in order from React.
useReducer :: forall s a i. (s -> a -> s) -> (i -> Effect s) -> i -> Hooks (Reducer s a)
useReducer f i j =
  hook $ \_ ->
    pure $ currySecond $ tuple $ react ... "useReducer" $ args3 (mkFn2 f) j (mkEffectFn1 i)

-- | Like `useReducer`, but takes an initial state instead of an
-- | initialiser function and argument
useReducer' :: forall s a. (s -> a -> s) -> s -> Hooks (Reducer s a)
useReducer' r = useReducer r pure

-- useContext

-- | Given a `Context`, returns its current value
useContext :: forall a. Context a -> Hooks a
useContext c = hook $ \_ -> pure $ react ... "useContext" $ [c]

-- useRef

useRef :: forall r. r -> Hooks (Ref r)
useRef r = hook $ \_ -> pure $ react ... "useRef" $ [ r ]

-- useDebugValue

useDebugValue :: forall v v'. v -> (v -> v') -> Hooks Unit
useDebugValue v f = hook $ \_ -> pure $ react ... "useDebugValue" $ (args2 v f)

useDebugValue' :: forall v. v -> Hooks Unit
useDebugValue' v = hook $ \_ -> pure $ react ... "useDebugValue" $ [v]

-- useEffect

type HookEffect = Unit -> Effect (Unit -> Effect Unit)

-- | A cleanup handler that does nothing
nothing :: Unit -> Effect Unit
nothing _ = pure unit

-- | Turns a simple effect function into an effect function that does
-- | nothing in cleanup after running
thenNothing :: forall a. (Unit -> Effect a) -> HookEffect
thenNothing e _ = e unit *> pure nothing

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

_useEffect :: forall a. HookEffect -> a -> Hooks Unit
_useEffect e a =
  hook $ \_ ->
    pure $ react ... "useEffect" $
      args2 (wrapEffect e) (Array.from a)

wrapEffect :: HookEffect -> Effect (Effect Unit)
wrapEffect f = delay unit $ \_ -> do
  cleanup <- f unit
  pure $ delay unit cleanup

-- | Like useEffect, but the provided Effect fn does not return a cleanup handler
useEffect' :: forall a. (Unit -> Effect a) -> Hooks Unit
useEffect' = useEffect <<< thenNothing

-- | Like useEffect1, but the provided Effect fn does not return a cleanup handler
useEffect1' :: forall a b. a -> (Unit -> Effect b) -> Hooks Unit
useEffect1' a = useEffect1 a <<< thenNothing

-- | Like useEffect2, but the provided Effect fn does not return a cleanup handler
useEffect2' :: forall a b c. a -> b -> (Unit -> Effect c) -> Hooks Unit
useEffect2' a b = useEffect2 a b <<< thenNothing

-- | Like useEffect3, but the provided Effect fn does not return a cleanup handler
useEffect3' :: forall a b c d. a -> b -> c -> (Unit -> Effect d) -> Hooks Unit
useEffect3' a b c = useEffect3 a b c <<< thenNothing

-- | Like useEffect4, but the provided Effect fn does not return a cleanup handler
useEffect4' :: forall a b c d e. a -> b -> c -> d -> (Unit -> Effect e) -> Hooks Unit
useEffect4' a b c d = useEffect4 a b c d <<< thenNothing

-- | Like useEffect5, but the provided Effect fn does not return a cleanup handler
useEffect5' :: forall a b c d e f. a -> b -> c -> d -> e -> (Unit -> Effect f) -> Hooks Unit
useEffect5' a b c d e = useEffect5 a b c d e <<< thenNothing

-- useLayoutEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called in the same phase as
-- | `componentDidMount` and `componentDidUpdate` used to be.
useLayoutEffect :: HookEffect -> Hooks Unit
useLayoutEffect e = hook $ \_ -> pure $ react ... "useLayoutEffect" $ [ wrapEffect e ]

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

-- | Like useLayoutEffect, but the provided Effect fn does not return a cleanup handler
useLayoutEffect' :: forall a. (Unit -> Effect a) -> Hooks Unit
useLayoutEffect' = useLayoutEffect <<< thenNothing

-- | Like useLayoutEffect1, but the provided Effect fn does not return a cleanup handler
useLayoutEffect1' :: forall a b. a -> (Unit -> Effect b) -> Hooks Unit
useLayoutEffect1' a = useLayoutEffect1 a <<< thenNothing

-- | Like useLayoutEffect2, but the provided Effect fn does not return a cleanup handler
useLayoutEffect2' :: forall a b c. a -> b -> (Unit -> Effect c) -> Hooks Unit
useLayoutEffect2' a b = useLayoutEffect2 a b <<< thenNothing

-- | Like useLayoutEffect3, but the provided Effect fn does not return a cleanup handler
useLayoutEffect3' :: forall a b c d. a -> b -> c -> (Unit -> Effect d) -> Hooks Unit
useLayoutEffect3' a b c = useLayoutEffect3 a b c <<< thenNothing

-- | Like useLayoutEffect4, but the provided Effect fn does not return a cleanup handler
useLayoutEffect4' :: forall a b c d e. a -> b -> c -> d -> (Unit -> Effect e) -> Hooks Unit
useLayoutEffect4' a b c d = useLayoutEffect4 a b c d <<< thenNothing

-- | Like useLayoutEffect5, but the provided Effect fn does not return a cleanup handler
useLayoutEffect5' :: forall a b c d e f. a -> b -> c -> d -> e -> (Unit -> Effect f) -> Hooks Unit
useLayoutEffect5' a b c d f = useLayoutEffect5 a b c d f <<< thenNothing

_useLayoutEffect :: forall a. HookEffect -> a -> Hooks Unit
_useLayoutEffect e a =
  hook $ \_ ->
    pure $ react ... "useLayoutEffect" $
      args2 (wrapEffect e) (Array.from a)

-- useMemo

-- | Given a function to compure an expensive value, returns the value
useMemo :: forall t. (Unit -> t) -> Hooks t
useMemo f = hook $ \_ -> pure $ react ... "useMemo" $ [ delay unit (\_ -> pure (f unit)) ]

-- | Like `useMemo` but takes a memo value
useMemo1 :: forall a t. a -> (Unit -> t) -> Hooks t
useMemo1 a f = _useMemo f [a]

-- | Like `useMemo` but takes 2 memo values
useMemo2 :: forall a b t. a -> b -> (Unit -> t) -> Hooks t
useMemo2 a b f = _useMemo f $ args2 a b

-- | Like `useMemo` but takes 3 memo values
useMemo3 :: forall a b c t. a -> b -> c -> (Unit -> t) -> Hooks t
useMemo3 a b c f = _useMemo f $ args3 a b c

-- | Like `useMemo` but takes 4 memo values
useMemo4 :: forall a b c d t. a -> b -> c -> d -> (Unit -> t) -> Hooks t
useMemo4 a b c d f = _useMemo f $ args4 a b c d

-- | Like `useMemo` but takes 5 memo values
useMemo5 :: forall a b c d e t. a -> b -> c -> d -> e -> (Unit -> t) -> Hooks t
useMemo5 a b c d e f = _useMemo f $ args5 a b c d e

_useMemo :: forall t a. (Unit -> t) -> a -> Hooks t
_useMemo f a =
  hook $ \_ ->
    pure $ react ... "useMemo" $
      args2 (delay unit (\_ -> pure (f unit))) (Array.from a)

-- useCallback

-- | Given a function to compure an expensive value, returns the value
useCallback :: forall t. (Unit -> t) -> Hooks (Effect t)
useCallback f = hook $ \_ -> pure $ react ... "useCallback" $ [ delay unit (\_ -> pure (f unit)) ]

-- | Like `useCallback` but takes a memo value
useCallback1 :: forall a t. a -> (Unit -> t) -> Hooks (Effect t)
useCallback1 a f = _useCallback f [a]

-- | Like `useCallback` but takes 2 memo values
useCallback2 :: forall a b t. a -> b -> (Unit -> t) -> Hooks (Effect t)
useCallback2 a b f = _useCallback f $ args2 a b

-- | Like `useCallback` but takes 3 memo values
useCallback3 :: forall a b c t. a -> b -> c -> (Unit -> t) -> Hooks (Effect t)
useCallback3 a b c f = _useCallback f $ args3 a b c

-- | Like `useCallback` but takes 4 memo values
useCallback4 :: forall a b c d t. a -> b -> c -> d -> (Unit -> t) -> Hooks (Effect t)
useCallback4 a b c d f = _useCallback f $ args4 a b c d

-- | Like `useCallback` but takes 5 memo values
useCallback5 :: forall a b c d e t. a -> b -> c -> d -> e -> (Unit -> t) -> Hooks (Effect t)
useCallback5 a b c d e f = _useCallback f $ args5 a b c d e

_useCallback :: forall t a. (Unit -> t) -> a -> Hooks (Effect t)
_useCallback f a =
  hook $ \_ ->
    pure $ react ... "useCallback" $
      args2 f (Array.from a)

-- useImperativeHandle

useImperativeHandle
  :: forall r r'
  .  Ref r -> (Unit -> Effect r') -> Hooks Unit
useImperativeHandle r f =
  hook $ \_ ->
    pure $ react ... "useImperativeHandle" $ args2 r f

useImperativeHandle1
  :: forall a r r'
  .  a -> Ref r -> (Unit -> Effect r') -> Hooks Unit
useImperativeHandle1 a r f = _useImperativeHandle r f [a]

useImperativeHandle2
  :: forall a b r r'
  .  a -> b -> Ref r -> (Unit -> Effect r') -> Hooks Unit
useImperativeHandle2 a b r f = _useImperativeHandle r f (args2 a b)

useImperativeHandle3
  :: forall a b c r r'
  .  a -> b -> c -> Ref r -> (Unit -> Effect r') -> Hooks Unit
useImperativeHandle3 a b c r f = _useImperativeHandle r f (args3 a b c)

useImperativeHandle4
  :: forall a b c d r r'
  .  a -> b -> c -> d -> Ref r -> (Unit -> Effect r') -> Hooks Unit
useImperativeHandle4 a b c d r f = _useImperativeHandle r f (args4 a b c d)

useImperativeHandle5
  :: forall a b c d e r r'
  .  a -> b -> c -> d -> e -> Ref r -> (Unit -> Effect r') -> Hooks Unit
useImperativeHandle5 a b c d e r f = _useImperativeHandle r f (args5 a b c d e)

_useImperativeHandle :: forall r r' a. Ref r -> (Unit -> Effect r') -> a -> Hooks Unit
_useImperativeHandle r f a = hook $ \_ ->
  pure $ react ... "useImperativeHandle" $
    args3 r (delay unit f) (Array.from a)
