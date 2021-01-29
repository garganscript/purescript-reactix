module Reactix.React
  ( React, react
  , ReactDOM, reactDOM, render
  , Element
  , Hooks, unsafeHooksEffect, runHooks

  , Context, createContext, provideContext, consumeContext
  , Provider, provider, provide
  , Consumer, consumer, consume
  , createPortal

  , class IsComponent
  , Component, createElement, createDOMElement
  , rawCreateElement
  , StaticComponent, staticComponent, staticComponentWithModule
  , HooksComponent, hooksComponent, hooksComponentWithModule
  , fragment

  , Ref, createRef, readRef, readRefM, readNullableRef, readNullableRefM, setRef

  , isValid

  , Memo, memo, memo'
  )
 where

import Prelude
import Control.Monad (Monad)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import DOM.Simple as DOM
import FFI.Simple.PseudoArray as PA
import FFI.Simple (args2, defineProperty, delay, (..), (...), (.=))

foreign import data React :: Type
foreign import data ReactDOM :: Type

foreign import react :: React
foreign import reactDOM :: ReactDOM

-- basic types

newtype Component props = Component (EffectFn1 (Record props) Element)

-- | A React Element node
foreign import data Element :: Type

-- | The Hooks monad
newtype Hooks a = Hooks (Effect a)

runHooks :: forall a. Hooks a -> Effect a
runHooks (Hooks a) = a

instance functorHooks :: Functor Hooks where
  map f (Hooks a) = Hooks (map f a)

instance applyHooks :: Apply Hooks where
  apply (Hooks f) (Hooks a) = Hooks (apply f a)

instance applicativeHooks :: Applicative Hooks where
  pure = Hooks <<< pure

instance bindHooks :: Bind Hooks where
  bind (Hooks a) f = Hooks (a >>= (runHooks <<< f))

instance monadHooks :: Monad Hooks

unsafeHooksEffect :: forall a. Effect a -> Hooks a
unsafeHooksEffect = Hooks

type DOMProps = ()

class IsComponent component (props :: # Type) children
  | component -> props, component -> children where
  createElement :: component -> Record props -> children -> Element

instance componentIsComponent :: IsComponent (Component props) props (Array Element) where
  createElement = rawCreateElement
instance memoIsComponent :: IsComponent (Memo props) props (Array Element) where
  createElement = rawCreateElement
instance providerIsComponent :: IsComponent (Provider v) (value :: v) (Array Element) where
  createElement = rawCreateElement
instance consumerIsComponent :: IsComponent (Consumer v) () (v -> Element) where
  createElement c p cs = rawCreateElement c p [cs]

-- Component building

-- | The type of a function that can be turned into a component with
-- | `staticComponent`. Will not have access to the `Hooks` Monad.

type Module = String

type StaticComponent props = Record props -> Array Element -> Element

-- | Turns a `StaticComponent` function into a Component
staticComponent :: forall props. String -> StaticComponent props -> Component props
staticComponent name c = Component $ named name $ mkEffectFn1 c'
  where
    c' :: Record props -> Effect Element
    c' props = pure $ c props (children props)

staticComponentWithModule :: forall props. Module -> String -> StaticComponent props -> Component props
staticComponentWithModule module' name c = staticComponent (module' <> "." <> name) c

-- | The type of a function that can be turned into a component with
-- | `hooksComponent`. Will have access to the `Hooks` Monad.
type HooksComponent props = Record props -> Array Element -> Hooks Element

-- | Turns a `HooksComponent` function into a Component
hooksComponent :: forall props. String -> HooksComponent props -> Component props
hooksComponent name c = Component $ named name $ mkEffectFn1 c'
  where
    c' :: Record props -> Effect Element
    c' props = runHooks $ c props (children props)

hooksComponentWithModule :: forall props. Module -> String -> HooksComponent props -> Component props
hooksComponentWithModule module' name c = hooksComponent (module' <> "." <> name) c

rawCreateElement :: forall c p cs. c -> p -> Array cs -> Element
rawCreateElement c p cs = react ... "createElement" $ args
   where args = PA.unshift c $ PA.unshift p cs

createDOMElement :: forall r. String -> Record r -> Array Element -> Element
createDOMElement = rawCreateElement

-- Element cloning

-- | Clones an element. Quite unsafe because tripping through Element
-- | loses the type of the props. Be careful.

-- cloneElement :: forall props. Element -> Record props -> Element
-- cloneElement e p = react ... "cloneElement" $ args2 e p

-- Fragment creation

-- TODO: add key support
-- | Combines several elements together
fragment :: Array Element -> Element
fragment es = rawCreateElement (react .. "Fragment") {} es

instance semigroupElement :: Semigroup Element where
  append a b = fragment [a, b]

instance monoidElement :: Monoid Element where
  mempty = fragment []

-- | Renders a React Element to a real Element
render :: Element -> DOM.Element -> Effect Unit
render e d = delay unit $ \_ -> pure $ reactDOM ... "render" $ args2 e d

createPortal :: Array Element -> DOM.Element -> Element
createPortal es e = reactDOM ... "createPortal" $ args2 es e



-- -- Memoisation

foreign import data Memo :: # Type -> Type

memo ::
  forall props.
     Component props
  -> (Record props -> Record props -> Boolean)
  -> Memo props
memo c f = react ... "memo" $ args2 c (mkFn2 f)

memo' :: forall props. Component props -> Memo props
memo' c = react ... "memo" $ [ c ]



-- Context

-- | A React Context
foreign import data Context :: Type -> Type

-- | The Provider for a React Context
foreign import data Provider :: Type -> Type

-- | The Consumer for a React Context
foreign import data Consumer :: Type -> Type

-- | Creates a `Context` from a given value
createContext :: forall v. v -> Context v
createContext v = react ... "createContext" $ [v]

provider :: forall v. Context v -> Provider v
provider c = c .. "Provider"

provide :: forall v. Provider v -> v -> Array Element -> Element
provide p v = rawCreateElement p { value: v }

provideContext :: forall v. Context v -> v -> Array Element -> Element
provideContext c = provide (provider c)

consumer :: forall v. Context v -> Consumer v
consumer c = c .. "Consumer"

consume :: forall v. Consumer v -> (v -> Element) -> Element
consume c f = rawCreateElement c {} [f]

consumeContext :: forall v. Context v -> (v -> Element) -> Element
consumeContext c = consume (consumer c)

-- Ref creation

foreign import data Ref :: Type -> Type

type NullableRef r = Ref (Nullable r)

createRef :: forall r. Unit -> NullableRef r
createRef _ = react ... "createRef" $ []

readRef :: forall r. Ref r -> r
readRef r = r .. "current"

readRefM :: forall r m. Monad m => Ref r -> m r
readRefM r = delay r (pure <<< readRef)

readNullableRef :: forall r. Ref (Nullable r) -> Maybe r
readNullableRef r = toMaybe $ r .. "current"

readNullableRefM :: forall r m. Monad m => Ref (Nullable r) -> Effect (Maybe r)
readNullableRefM r = delay r (pure <<< readNullableRef)

setRef :: forall r. Ref r -> r -> Effect Unit
setRef r v = delay unit $ \_ -> (pure $ r .= "current" $ v) *> pure unit

-- Ref Forwarding

-- forwardRef :: forall p (p ->

-- foreign import data Forwarded :: Type -> Type

-- foreign import _forwardRef :: forall r p. (Fn2 p r Element) -> Forwarded p

named
  :: forall props
  .  String
  -> EffectFn1 (Record props) Element
  -> EffectFn1 (Record props) Element
named = flip $ defineProperty "name"

isValid :: forall a. a -> Boolean
isValid a = react ... "isValidElement" $ [ a ]

-- Utils

children :: forall a. a -> Array Element
children a = react .. "Children" ... "toArray" $ [ (a .. "children") ]
