module Reactix.Hooks
  ( State, Setter, useState, useState'
  , Reducer, useReducer, useReducer'
  , useContext
  , useRef
  , useDebugValue, useDebugValue'
  , nothing, thenNothing
  , useEffectOnce, useEffectOnce'
  , useEffect,  useEffect'
  , useEffect1, useEffect1', useEffectFn1, useEffectFn1'
  , useEffect2, useEffect2', useEffectFn2, useEffectFn2'
  , useEffect3, useEffect3', useEffectFn3, useEffectFn3'
  , useEffect4, useEffect4', useEffectFn4, useEffectFn4'
  , useEffect5, useEffect5', useEffectFn5, useEffectFn5'
  , unsafeUseEffect
  , useLayoutEffect,  useLayoutEffect'
  , useLayoutEffect1, useLayoutEffect1', useLayoutEffectFn1, useLayoutEffectFn1'
  , useLayoutEffect2, useLayoutEffect2', useLayoutEffectFn2, useLayoutEffectFn2'
  , useLayoutEffect3, useLayoutEffect3', useLayoutEffectFn3, useLayoutEffectFn3'
  , useLayoutEffect4, useLayoutEffect4', useLayoutEffectFn4, useLayoutEffectFn4'
  , useLayoutEffect5, useLayoutEffect5', useLayoutEffectFn5, useLayoutEffectFn5'
  , unsafeUseLayoutEffect
  , useMemo, useMemo1, useMemo2, useMemo3, useMemo4, useMemo5
  , unsafeUseMemo
  , useCallback,  useCallback1, useCallback2
  , useCallback3, useCallback4, useCallback5
  , unsafeUseCallback
  , useImperativeHandle,  useImperativeHandle1, useImperativeHandle2
  , useImperativeHandle3, useImperativeHandle4, useImperativeHandle5
  , unsafeUseImperativeHandle
  )
 where

import Prelude (Unit, const, identity, pure, unit, ($), (*>), (<<<))
import Data.Function.Uncurried (mkFn2)
import Data.Tuple (Tuple)
import Effect (Effect)
import FFI.Simple (args2, args3, args4, args5, delay, (...))
import FFI.Simple.PseudoArray as Array
import Reactix.Utils (tuple, currySecond, hook, splay1, splay2, splay3, splay4, splay5)
import Reactix.React (Context, Hooks, Ref, react)

--- useState

-- delay concretely typed to Effect
delayEffect :: forall a b. a -> (a -> Effect b) -> Effect b
delayEffect = delay

-- | A state hook is a tuple of value and setter
type State state = Tuple state ((state -> state) -> Effect Unit)

-- a setter function, for useState
type Setter t = (t -> t) -> Effect Unit

-- | Given an Effect function returning an initial value, returns a State
useState :: forall s. (Unit -> s) -> Hooks (State s)
useState s =
  hook $ \_ ->
    pure $ currySecond $ tuple $ react ... "useState" $ [ delayEffect unit (pure <<< s) ]

useState' :: forall s. s -> Hooks (State s)
useState' = useState <<< const
-- useReducer

-- | A reducer hook is a tuple of value and reducer-setter
type Reducer state action = Tuple state (action -> Effect Unit)

-- | Given a reducer function from a state and action to a new state,
-- | an initialiser function and argument for the initialiser, returns
-- | a Reducer. Note args 2 and 3 are swapped in order from React.
useReducer :: forall s a i. (s -> a -> s) -> (i -> s) -> i -> Hooks (Reducer s a)
useReducer f i j =
  hook $ \_ ->
    pure $ currySecond $ tuple $ react ... "useReducer" $ args3 (mkFn2 f) j i

-- | Like `useReducer`, but takes an initial state instead of an
-- | initialiser function and argument
useReducer' :: forall s a. (s -> a -> s) -> s -> Hooks (Reducer s a)
useReducer' r = useReducer r identity

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

-- | A cleanup handler that does nothing
nothing :: Effect Unit
nothing = pure unit

-- | Turns a simple effect function into an effect function that does
-- | nothing in cleanup after running
thenNothing :: forall a. Effect a -> Effect (Effect Unit)
thenNothing e = e *> pure nothing

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called after rendering
useEffect :: Effect (Effect Unit) -> Hooks Unit
useEffect e = hook $ \_ -> pure $ react ... "useEffect" $ [e]

-- | Like useEffect, but runs only once in the lifecycle of the
-- | component The initial effect will be fired at mount and the
-- | cleanup effect will be fired at unmount
useEffectOnce :: Effect (Effect Unit) -> Hooks Unit
useEffectOnce e = unsafeUseEffect e []

-- | Like useEffect, but with a memo value
useEffect1 :: forall a. a -> Effect (Effect Unit) -> Hooks Unit
useEffect1 a e = unsafeUseEffect e [a]

-- | Like useEffect, but with 2 memo values
useEffect2 :: forall a b. a -> b -> Effect (Effect Unit) -> Hooks Unit
useEffect2 a b e = unsafeUseEffect e $ args2 a b

-- | Like useEffect, but with 3 memo values
useEffect3 :: forall a b c. a -> b -> c -> Effect (Effect Unit) -> Hooks Unit
useEffect3 a b c e = unsafeUseEffect e $ args3 a b c

-- | Like useEffect, but with 4 memo values
useEffect4 :: forall a b c d. a -> b -> c -> d -> Effect (Effect Unit) -> Hooks Unit
useEffect4 a b c d e = unsafeUseEffect e $ args4 a b c d

-- | Like useEffect, but with 5 memo values
useEffect5 :: forall a b c d e. a -> b -> c -> d -> e -> Effect (Effect Unit) -> Hooks Unit
useEffect5 a b c d f e = unsafeUseEffect e $ args5 a b c d f

-- | Call React.useEffect passing an array-like object (arguments is
-- | acceptable) for memo values without any help from the type system
-- | to guarantee the memo value is in fact array-like.
unsafeUseEffect :: forall a. Effect (Effect Unit) -> a -> Hooks Unit
unsafeUseEffect e a =
  hook $ \_ ->
    pure $ react ... "useEffect" $
      args2 e (Array.from a)

-- | Like useEffectOnce, but the provided Effect fn does not return a cleanup handler
useEffectOnce' :: forall a. Effect a -> Hooks Unit
useEffectOnce' = useEffectOnce <<< thenNothing

-- | Like useEffect, but the provided Effect fn does not return a cleanup handler
useEffect' :: forall a. Effect a -> Hooks Unit
useEffect' = useEffect <<< thenNothing

-- | Like useEffect1, but the provided Effect fn does not return a cleanup handler
useEffect1' :: forall a b. a -> Effect b -> Hooks Unit
useEffect1' a = useEffect1 a <<< thenNothing

-- | Like useEffect2, but the provided Effect fn does not return a cleanup handler
useEffect2' :: forall a b c. a -> b -> Effect c -> Hooks Unit
useEffect2' a b = useEffect2 a b <<< thenNothing

-- | Like useEffect3, but the provided Effect fn does not return a cleanup handler
useEffect3' :: forall a b c d. a -> b -> c -> Effect d -> Hooks Unit
useEffect3' a b c = useEffect3 a b c <<< thenNothing

-- | Like useEffect4, but the provided Effect fn does not return a cleanup handler
useEffect4' :: forall a b c d e. a -> b -> c -> d -> Effect e -> Hooks Unit
useEffect4' a b c d = useEffect4 a b c d <<< thenNothing

-- | Like useEffect5, but the provided Effect fn does not return a cleanup handler
useEffect5' :: forall a b c d e f. a -> b -> c -> d -> e -> Effect f -> Hooks Unit
useEffect5' a b c d e = useEffect5 a b c d e <<< thenNothing

-- | Like useEffect1, but takes a function from memo value to effect
useEffectFn1 :: forall a. a -> (a -> Effect (Effect Unit)) -> Hooks Unit
useEffectFn1 a f = splay1 useEffect1 f a

-- | like useEffectFn1, but with two memo values
useEffectFn2 :: forall a b. a -> b -> (a -> b -> Effect (Effect Unit)) -> Hooks Unit
useEffectFn2 a b f = splay2 useEffect2 f a b

-- | like useEffectFn1, but with three memo values
useEffectFn3 :: forall a b c. a -> b -> c -> (a -> b -> c -> Effect (Effect Unit)) -> Hooks Unit
useEffectFn3 a b c f = splay3 useEffect3 f a b c

-- | like useEffectFn1, but with four memo values
useEffectFn4 :: forall a b c d. a -> b -> c -> d -> (a -> b -> c -> d -> Effect (Effect Unit)) -> Hooks Unit
useEffectFn4 a b c d f = splay4 useEffect4 f a b c d

-- | like useEffectFn1, but with five memo values
useEffectFn5 :: forall a b c d e. a -> b -> c -> d -> e -> (a -> b -> c -> d -> e -> Effect (Effect Unit)) -> Hooks Unit
useEffectFn5 a b c d e f = splay5 useEffect5 f a b c d e

-- | Like useEffect1', but takes a function from memo value to effect
useEffectFn1' :: forall a. a -> (a -> Effect Unit) -> Hooks Unit
useEffectFn1' a f = splay1 useEffect1' f a

-- | like useEffectFn1', but with two memo values
useEffectFn2' :: forall a b. a -> b -> (a -> b -> Effect Unit) -> Hooks Unit
useEffectFn2' a b f = splay2 useEffect2' f a b

-- | like useEffectFn1', but with three memo values
useEffectFn3' :: forall a b c. a -> b -> c -> (a -> b -> c -> Effect Unit) -> Hooks Unit
useEffectFn3' a b c f = splay3 useEffect3' f a b c

-- | like useEffectFn1', but with four memo values
useEffectFn4' :: forall a b c d. a -> b -> c -> d -> (a -> b -> c -> d -> Effect Unit) -> Hooks Unit
useEffectFn4' a b c d f = splay4 useEffect4' f a b c d

-- | like useEffectFn1', but with five memo values
useEffectFn5' :: forall a b c d e. a -> b -> c -> d -> e -> (a -> b -> c -> d -> e -> Effect Unit) -> Hooks Unit
useEffectFn5' a b c d e f = splay5 useEffect5' f a b c d e


-- useLayoutEffect

-- | Given an Effect function which returns a cleanup Effect function,
-- | register an effect to be called in the same phase as
-- | `componentDidMount` and `componentDidUpdate` used to be.
useLayoutEffect :: Effect (Effect Unit) -> Hooks Unit
useLayoutEffect e = hook $ \_ -> pure $ react ... "useLayoutEffect" $ [ e ]

-- | Like useLayoutEffect, but with a memo value
useLayoutEffect1 :: forall a. a -> Effect (Effect Unit) -> Hooks Unit
useLayoutEffect1 a e = unsafeUseLayoutEffect e [a]

-- | Like useLayoutEffect, but with 2 memo values
useLayoutEffect2 :: forall a b. a -> b -> Effect (Effect Unit) -> Hooks Unit
useLayoutEffect2 a b e = unsafeUseLayoutEffect e $ args2 a b

-- | Like useLayoutEffect, but with 3 memo values
useLayoutEffect3 :: forall a b c. a -> b -> c -> Effect (Effect Unit) -> Hooks Unit
useLayoutEffect3 a b c e = unsafeUseLayoutEffect e $ args3 a b c

-- | Like useLayoutEffect, but with 4 memo values
useLayoutEffect4 :: forall a b c d. a -> b -> c -> d -> Effect (Effect Unit) -> Hooks Unit
useLayoutEffect4 a b c d e = unsafeUseLayoutEffect e $ args4 a b c d

-- | Like useLayoutEffect, but with 5 memo values
useLayoutEffect5 :: forall a b c d e. a -> b -> c -> d -> e -> Effect (Effect Unit) -> Hooks Unit
useLayoutEffect5 a b c d f e = unsafeUseLayoutEffect e $ args5 a b c d f

-- | Like useLayoutEffect, but the provided Effect fn does not return a cleanup handler
useLayoutEffect' :: forall a. Effect a -> Hooks Unit
useLayoutEffect' = useLayoutEffect <<< thenNothing

-- | Like useLayoutEffect1, but the provided Effect fn does not return a cleanup handler
useLayoutEffect1' :: forall a b. a -> Effect b -> Hooks Unit
useLayoutEffect1' a = useLayoutEffect1 a <<< thenNothing

-- | Like useLayoutEffect1' but with 2 memo values
useLayoutEffect2' :: forall a b c. a -> b -> Effect c -> Hooks Unit
useLayoutEffect2' a b = useLayoutEffect2 a b <<< thenNothing

-- | Like useLayoutEffect1' but with 3 memo values
useLayoutEffect3' :: forall a b c d. a -> b -> c -> Effect d -> Hooks Unit
useLayoutEffect3' a b c = useLayoutEffect3 a b c <<< thenNothing

-- | Like useLayoutEffect1' but with 4 memo values
useLayoutEffect4' :: forall a b c d e. a -> b -> c -> d -> Effect e -> Hooks Unit
useLayoutEffect4' a b c d = useLayoutEffect4 a b c d <<< thenNothing

-- | Like useLayoutEffect1' but with 5 memo values
useLayoutEffect5' :: forall a b c d e f. a -> b -> c -> d -> e -> Effect f -> Hooks Unit
useLayoutEffect5' a b c d f = useLayoutEffect5 a b c d f <<< thenNothing

-- | Call React.useLayoutEffect passing an array-like object
-- | (arguments is acceptable) for memo values without any help from
-- | the type system to guarantee the memo value is in fact array-like.
unsafeUseLayoutEffect :: forall a. Effect (Effect Unit) -> a -> Hooks Unit
unsafeUseLayoutEffect e a =
  hook $ \_ ->
    pure $ react ... "useLayoutEffect" $
      args2 e (Array.from a)

-- | Like useLayoutEffect1, but takes a function from memo value to effect
useLayoutEffectFn1 :: forall a. a -> (a -> Effect (Effect Unit)) -> Hooks Unit
useLayoutEffectFn1 a f = splay1 useLayoutEffect1 f a

-- | like useLayoutEffectFn1, but with two memo values
useLayoutEffectFn2 :: forall a b. a -> b -> (a -> b -> Effect (Effect Unit)) -> Hooks Unit
useLayoutEffectFn2 a b f = splay2 useLayoutEffect2 f a b

-- | like useLayoutEffectFn1, but with three memo values
useLayoutEffectFn3 :: forall a b c. a -> b -> c -> (a -> b -> c -> Effect (Effect Unit)) -> Hooks Unit
useLayoutEffectFn3 a b c f = splay3 useLayoutEffect3 f a b c

-- | like useLayoutEffectFn1, but with four memo values
useLayoutEffectFn4 :: forall a b c d. a -> b -> c -> d -> (a -> b -> c -> d -> Effect (Effect Unit)) -> Hooks Unit
useLayoutEffectFn4 a b c d f = splay4 useLayoutEffect4 f a b c d

-- | like useLayoutEffectFn1, but with five memo values
useLayoutEffectFn5 :: forall a b c d e. a -> b -> c -> d -> e -> (a -> b -> c -> d -> e -> Effect (Effect Unit)) -> Hooks Unit
useLayoutEffectFn5 a b c d e f = splay5 useLayoutEffect5 f a b c d e

-- | Like useLayoutEffect1, but takes a function from memo value to effect
useLayoutEffectFn1' :: forall a. a -> (a -> Effect Unit) -> Hooks Unit
useLayoutEffectFn1' a f = splay1 useLayoutEffect1' f a

-- | like useLayoutEffectFn1', but with two memo values
useLayoutEffectFn2' :: forall a b. a -> b -> (a -> b -> Effect Unit) -> Hooks Unit
useLayoutEffectFn2' a b f = splay2 useLayoutEffect2' f a b

-- | like useLayoutEffectFn1', but with three memo values
useLayoutEffectFn3' :: forall a b c. a -> b -> c -> (a -> b -> c -> Effect Unit) -> Hooks Unit
useLayoutEffectFn3' a b c f = splay3 useLayoutEffect3' f a b c

-- | like useLayoutEffectFn1', but with four memo values
useLayoutEffectFn4' :: forall a b c d. a -> b -> c -> d -> (a -> b -> c -> d -> Effect Unit) -> Hooks Unit
useLayoutEffectFn4' a b c d f = splay4 useLayoutEffect4' f a b c d

-- | like useLayoutEffectFn1', but with five memo values
useLayoutEffectFn5' :: forall a b c d e. a -> b -> c -> d -> e -> (a -> b -> c -> d -> e -> Effect Unit) -> Hooks Unit
useLayoutEffectFn5' a b c d e f = splay5 useLayoutEffect5' f a b c d e



-- useMemo

-- | Given a function to compute an expensive value, returns the value
useMemo :: forall t. (Unit -> t) -> Hooks t
useMemo f = hook $ \_ -> pure $ react ... "useMemo" $ [ delayEffect unit (\_ -> pure (f unit)) ]

-- | Like `useMemo` but takes a memo value
useMemo1 :: forall a t. a -> (Unit -> t) -> Hooks t
useMemo1 a f = unsafeUseMemo f [a]

-- | Like `useMemo` but takes 2 memo values
useMemo2 :: forall a b t. a -> b -> (Unit -> t) -> Hooks t
useMemo2 a b f = unsafeUseMemo f $ args2 a b

-- | Like `useMemo` but takes 3 memo values
useMemo3 :: forall a b c t. a -> b -> c -> (Unit -> t) -> Hooks t
useMemo3 a b c f = unsafeUseMemo f $ args3 a b c

-- | Like `useMemo` but takes 4 memo values
useMemo4 :: forall a b c d t. a -> b -> c -> d -> (Unit -> t) -> Hooks t
useMemo4 a b c d f = unsafeUseMemo f $ args4 a b c d

-- | Like `useMemo` but takes 5 memo values
useMemo5 :: forall a b c d e t. a -> b -> c -> d -> e -> (Unit -> t) -> Hooks t
useMemo5 a b c d e f = unsafeUseMemo f $ args5 a b c d e

-- | Call React.useMemo passing an array-like object (arguments is
-- | acceptable) for memo values without any help from the type system
-- | to guarantee the memo value is in fact array-like.
unsafeUseMemo :: forall t a. (Unit -> t) -> a -> Hooks t
unsafeUseMemo f a =
  hook $ \_ ->
    pure $ react ... "useMemo" $
      args2 (delayEffect unit (\_ -> pure (f unit))) (Array.from a)

-- useCallback

-- | Given a function to compute an expensive value, returns the value
useCallback :: forall t. (Unit -> t) -> Hooks (Effect t)
useCallback f = hook $ \_ -> pure $ react ... "useCallback" $ [ delayEffect unit (\_ -> pure (f unit)) ]

-- | Like `useCallback` but takes a memo value
useCallback1 :: forall a t. a -> (Unit -> t) -> Hooks (Effect t)
useCallback1 a f = unsafeUseCallback f [a]

-- | Like `useCallback` but takes 2 memo values
useCallback2 :: forall a b t. a -> b -> (Unit -> t) -> Hooks (Effect t)
useCallback2 a b f = unsafeUseCallback f $ args2 a b

-- | Like `useCallback` but takes 3 memo values
useCallback3 :: forall a b c t. a -> b -> c -> (Unit -> t) -> Hooks (Effect t)
useCallback3 a b c f = unsafeUseCallback f $ args3 a b c

-- | Like `useCallback` but takes 4 memo values
useCallback4 :: forall a b c d t. a -> b -> c -> d -> (Unit -> t) -> Hooks (Effect t)
useCallback4 a b c d f = unsafeUseCallback f $ args4 a b c d

-- | Like `useCallback` but takes 5 memo values
useCallback5 :: forall a b c d e t. a -> b -> c -> d -> e -> (Unit -> t) -> Hooks (Effect t)
useCallback5 a b c d e f = unsafeUseCallback f $ args5 a b c d e

-- | Call React.useCallback passing an array-like object (arguments is
-- | acceptable) for memo values without any help from the type system
-- | to guarantee the memo value is in fact array-like.
unsafeUseCallback :: forall t a. (Unit -> t) -> a -> Hooks (Effect t)
unsafeUseCallback f a =
  hook $ \_ ->
    pure $ react ... "useCallback" $
      args2 f (Array.from a)

-- useImperativeHandle

useImperativeHandle :: forall r r'.  Ref r -> Effect r' -> Hooks Unit
useImperativeHandle r f =
  hook $ \_ ->
    pure $ react ... "useImperativeHandle" $ args2 r f

useImperativeHandle1 :: forall a r r'.  a -> Ref r -> Effect r' -> Hooks Unit
useImperativeHandle1 a r f = unsafeUseImperativeHandle r f [a]

useImperativeHandle2
  :: forall a b r r'
  .  a -> b -> Ref r -> Effect r' -> Hooks Unit
useImperativeHandle2 a b r f = unsafeUseImperativeHandle r f (args2 a b)

useImperativeHandle3
  :: forall a b c r r'
  .  a -> b -> c -> Ref r -> Effect r' -> Hooks Unit
useImperativeHandle3 a b c r f = unsafeUseImperativeHandle r f (args3 a b c)

useImperativeHandle4
  :: forall a b c d r r'
  .  a -> b -> c -> d -> Ref r -> Effect r' -> Hooks Unit
useImperativeHandle4 a b c d r f = unsafeUseImperativeHandle r f (args4 a b c d)

useImperativeHandle5
  :: forall a b c d e r r'
  .  a -> b -> c -> d -> e -> Ref r -> Effect r' -> Hooks Unit
useImperativeHandle5 a b c d e r f = unsafeUseImperativeHandle r f (args5 a b c d e)

-- | Call React.useImperativeHandle passing an array-like object (arguments is
-- | acceptable) for memo values without any help from the type system
-- | to guarantee the memo value is in fact array-like.
unsafeUseImperativeHandle :: forall r r' a. Ref r -> Effect r' -> a -> Hooks Unit
unsafeUseImperativeHandle r f a = hook $ \_ ->
  pure $ react ... "useImperativeHandle" $
    args3 r f (Array.from a)
