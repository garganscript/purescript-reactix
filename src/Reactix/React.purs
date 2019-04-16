module Reactix.React
  ( Element, cloneElement, createDOMElement
  , Children, children
  , class Childless
  , class MonadHooks, runHooks
  , Context, ContextProvider, ContextConsumer, createContext, provider, consumer

  , Component
  , pureLeaf, pureTree, hooksLeaf, hooksTree

  , fragment

  , DOMElement
  , NullableRef, createRef

  -- , class Read, read
  -- , class Write, write
  )
 where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2, mkFn2, Fn3, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Lacks)

-- basic types

-- | A React Element node
foreign import data Element :: Type

-- | A wrapper over an Array of Elements
foreign import data Children :: Type

-- | A live DOM element rendered in the browser
foreign import data DOMElement :: Type

-- | A convenience for adding `children` to a list of props
type WithChildren p = ( children :: Children | p )

-- | This is to hide that it's actually implemented with Effect
class Monad m <= MonadHooks m where
  runHooks :: forall a. m a -> Effect a

instance monadHooksEffect :: MonadHooks Effect where
  runHooks = identity
  
class Childless (props :: # Type)

instance childlessLacksChildren :: Lacks "children" props => Childless props

newtype Component p = Component (EffectFn1 (Record p) Element)

-- instance createElementFnComponent :: CreateElement (FunctionComponent p) p

-- | Things which can be read purely
class Read r v where
  read :: r -> v

foreign import data Memo :: # Type -> Type

-- Component building

-- | Creates a pure leaf component from a function
pureLeaf ::
  forall props. Childless props
  => (Record props -> Element)
  -> Component props
pureLeaf f = Component (mkEffectFn1 $ pure <<< f)

-- | Creates a pure tree component from a function
pureTree ::
  forall props. Childless props
  => (Record props -> Array Element -> Element)
  -> Component (WithChildren props)
pureTree c = Component $ mkEffectFn1 c' 
  where
    c' :: Record (WithChildren props) -> Effect Element
    c' props = pure $ c (unsafeCoerce props) (children props.children)

-- | Creates a hooks leaf component from a function
hooksLeaf ::
  forall props m.
     MonadHooks m
  => Childless props
  => (Record props -> m Element)
  -> Component props
hooksLeaf c = Component (mkEffectFn1 $ runHooks <<< c)

hooksTree ::
  forall props m.
     MonadHooks m
  => Childless props
  => (Record props -> Array Element -> m Element)
  -> Component (WithChildren props)
hooksTree c = Component $ mkEffectFn1 c'
  where
    c' :: Record (WithChildren props) -> Effect Element
    c' props = runHooks $ c (unsafeCoerce props) (children props.children)


-- element creation

-- | Creates a DOM element of the given tag
createDOMElement :: forall props. String -> Record props -> Array Element -> Element
createDOMElement = runFn3 _createElement

-- | Creates a leaf component from a props Record
createLeaf ::
  forall tree props.
     Childless props
  => Component props
  -> Record props
  -> Element
createLeaf c p = runFn3 _createElement c p []

-- | Creates a tree component from a props Record and an Array of children
createTree ::
  forall props.
     Childless props
  => Component (WithChildren props)
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

instance readNullableRef :: Read (NullableRef r) (Maybe r) where
  read = toMaybe <<< _deref


-- Ref Forwarding

-- forwardRef :: forall p (p ->

-- foreign import data Forwarded :: Type -> Type

-- foreign import _forwardRef :: forall r p. (Fn2 p r Element) -> Forwarded p


