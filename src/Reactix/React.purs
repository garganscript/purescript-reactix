module Reactix.React
  ( Element, cloneElement, createDOMElement
  , Children, children
  , class Childless
  , class MonadHooks, unsafeHooksEffect
  , Context, ContextProvider, ContextConsumer, createContext, provider, consumer

  , render
  
  , Component
  , pureLeaf, pureTree, hooksLeaf, hooksTree
  , class Componentesque
  , createLeaf, createTree
  , fragment

  , NullableRef, createRef

  , isValid

  , Memo
  , memo, memo'
  )
 where

import Prelude
import Data.Function.Uncurried ( Fn2, runFn2, mkFn2, Fn3, runFn3 )
import Data.Maybe ( Maybe )
import Data.Nullable ( Nullable, toMaybe )
import Effect ( Effect )
import Effect.Class ( class MonadEffect, liftEffect )
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Lacks)
import DOM.Simple as DOM

-- basic types

-- | A React Element node
foreign import data Element :: Type

-- | A wrapper over an Array of Elements
foreign import data Children :: Type


-- | A convenience for adding `children` to a list of props
type WithChildren p = ( children :: Children | p )

-- | This is to hide that it's actually implemented with Effect
class Monad m <= MonadHooks m where
  unsafeHooksEffect :: forall a. Effect a -> m a

instance monadHooksEffect :: MonadHooks Effect where
  unsafeHooksEffect = unsafeCoerce
  
class Childless (props :: # Type)

instance childlessLacksChildren :: Lacks "children" props => Childless props

class Componentesque (c :: # Type -> Type)

newtype Component p = Component (EffectFn1 (Record p) Element)

instance componentesqueComponent :: Componentesque Component

foreign import data Memo :: # Type -> Type

instance componentesqueMemo :: Componentesque Memo

-- Component building

-- | Creates a pure leaf component from a function
pureLeaf ::
  forall props. Childless props
  => String
  -> (Record props -> Element)
  -> Component props
pureLeaf name f = named name $ Component (mkEffectFn1 $ pure <<< f)

-- | Creates a pure tree component from a function
pureTree ::
  forall props. Childless props
  => String
  -> (Record props -> Array Element -> Element)
  -> Component (WithChildren props)
pureTree name c = named name $ Component $ mkEffectFn1 c' 
  where
    c' :: Record (WithChildren props) -> Effect Element
    c' props = pure $ c (unsafeCoerce props) (children props.children)

-- | Creates a hooks leaf component from a function
hooksLeaf ::
  forall props. Childless props
  => String
  -> (forall m. MonadHooks m => Record props -> m Element)
  -> Component props
hooksLeaf name c = named name $ Component (mkEffectFn1 c)

hooksTree ::
  forall props. Childless props
  => String
  -> (forall m. MonadHooks m
      => Record props
      -> Array Element
      -> m Element)
  -> Component (WithChildren props)
hooksTree name c = named name $ Component $ mkEffectFn1 c'
  where
    c' :: Record (WithChildren props) -> Effect Element
    c' props = c (unsafeCoerce props) (children props.children)


-- element creation

-- | Creates a DOM element of the given tag
createDOMElement :: forall props. String -> Record props -> Array Element -> Element
createDOMElement = runFn3 _createElement

-- | Creates a leaf component from a props Record
createLeaf ::
  forall props cpt.
     Childless props
  => Componentesque cpt
  => cpt props
  -> Record props
  -> Element
createLeaf c p = runFn3 _createElement c p []

-- | Creates a tree component from a props Record and an Array of children
createTree ::
  forall props cpt.
     Childless props
  => Componentesque cpt
  => cpt (WithChildren props)
  -> Record props
  -> Array Element
  -> Element
createTree = runFn3 _createElement

-- createElement :: forall c p. CreateElement c p => c -> p -> Array Element ->

foreign import _createElement :: forall c p cs e. Fn3 c p cs e



-- Element cloning

-- | Clones an element, 

cloneElement :: forall props. Element -> Record props -> Element
cloneElement = runFn2 _cloneElement

foreign import _cloneElement :: forall p. Fn2 Element p Element

-- Fragment creation

-- | Combines several elements together
fragment :: Array Element -> Element
fragment = _createFragment

foreign import _createFragment :: Array Element -> Element

instance semigroupElement :: Semigroup Element where
  append a b = fragment [a, b]

-- | Renders a React Element to a real Element
render :: forall m. MonadEffect m => MonadHooks m => Element -> DOM.Element -> m Unit
render e d = liftEffect (runEffectFn2 _render e d)

foreign import _render :: EffectFn2 Element DOM.Element Unit


-- Memoisation

memo ::
  forall props.
     Component props
  -> (Record props -> Record props -> Boolean)
  -> Memo props
memo c f = runFn2 _memo c (mkFn2 f)

memo' :: forall props. Component props -> Memo props
memo' = _memoPrime

foreign import _memo :: forall c f r. Fn2 c f r
foreign import _memoPrime :: forall c r. c -> r



-- Children

foreign import _children :: Children -> Array Element

children :: Children -> Array Element
children = _children



-- Context

-- | A React Context
foreign import data Context :: Type -> Type

-- | The Provider for a React Context
foreign import data ContextProvider :: Type -> Type

-- | The Consumer for a React Context
foreign import data ContextConsumer :: Type -> Type

foreign import _createContext :: forall v. v -> Context v
foreign import _contextProvider :: forall v. Context v -> ContextProvider v
foreign import _contextConsumer :: forall v. Context v -> ContextConsumer v

createContext :: forall v. v -> Context v
createContext = _createContext

provider :: forall v. Context v -> ContextProvider v
provider = _contextProvider

consumer :: forall v. Context v -> ContextConsumer v
consumer = _contextConsumer



-- Ref creation

foreign import data NullableRef :: Type -> Type

foreign import _createRef :: forall r. Unit -> NullableRef r

createRef :: forall r. Unit -> NullableRef r
createRef = _createRef

foreign import _deref :: forall r. NullableRef r -> Nullable r

readNullableRef :: forall r. NullableRef r -> Maybe r
readNullableRef = toMaybe <<< _deref


-- Ref Forwarding

-- forwardRef :: forall p (p ->

-- foreign import data Forwarded :: Type -> Type

-- foreign import _forwardRef :: forall r p. (Fn2 p r Element) -> Forwarded p

named :: forall c. String -> c -> c
named = runFn2 _named

foreign import _named :: forall c. Fn2 String c c

foreign import _isValid :: forall a. a -> Boolean

isValid :: forall a. a -> Boolean
isValid = _isValid
