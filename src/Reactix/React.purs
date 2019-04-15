module Reactix.React
  ( Element, cloneElement
  , Children, children
  , Hooks, runHooks
  , Context, ContextProvider, ContextConsumer, createContext, provider, consumer

  , class Component

  , class LeafComponent
  , Leaf, createLeaf
  , MemoLeaf, memoLeaf, memoLeaf'

  , class TreeComponent
  , Tree, createTree

  , fragment
  , MemoTree, memoTree, memoTree'

  , DOMElement
  , NullableRef, createRef

  , class Read, read
  , class Write, write
  )
 where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
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

-- polymorphism

class Component c (p :: # Type) | c -> p

-- | Components which do not have children
class Component c p <= LeafComponent c (p :: # Type) | c -> p

-- | Components which may have children
class Component c p <= TreeComponent c (p :: # Type) | c -> p

-- | Things which can be read purely
class Read r v where
  read :: r -> v

-- | Things which can be written in Effect
class Write w v where
  write :: w -> v -> Effect Unit

-- Leaves and trees

-- | A component which does not accept children
newtype Leaf props = Leaf (Record props -> Element)

instance componentLeaf :: Component (Leaf p) p
instance leafComponentLeaf :: LeafComponent (Leaf p) p

-- | A component which accepts children
newtype Tree props = Tree (Record (WithChildren props) -> Element)

instance componentTree :: Component (Tree p) p
instance treeComponentTree :: TreeComponent (Tree p) p



-- Memoised leaves and trees

-- | A memoised Leaf
foreign import data MemoLeaf :: # Type -> Type

instance componentMemoLeaf :: Component (MemoLeaf p) p
instance leafComponentMemoLeaf :: LeafComponent (MemoLeaf p) p

-- | A memoised Tree
foreign import data MemoTree :: # Type -> Type

instance componentMemoTree :: Component (MemoTree p) p
instance treeComponentMemoTree :: TreeComponent (MemoTree p) p





-- Hooks

-- | The Hooks monad. We suspect it is the Identity monad in disguise
newtype Hooks a = Hooks (Unit -> a)

-- | Sequences a Hooks computation and returns the result
-- | Warning: must not be used outside of a react component
-- | `hooksLeaf` and `hooksTree` handle this for you, prefer them.
runHooks :: forall a. Hooks a -> a
runHooks (Hooks a) = a unit

instance functorHooks :: Functor Hooks where
  map f (Hooks a) = Hooks (\_ -> f (a unit))

instance applyHooks :: Apply Hooks where
  apply (Hooks f) (Hooks a) = Hooks (\_ -> (f unit) (a unit))

instance applicativeHooks :: Applicative Hooks where
  pure a = Hooks (\_ -> a)

instance bindHooks :: Bind Hooks where
  bind (Hooks a) f = f (a unit)

instance monadHooks :: Monad Hooks




-- Component building

-- | Creates a pure leaf component from a function
pureLeaf ::
  forall props. Lacks "children" props
  => (Record props -> Element)
  -> Leaf props
pureLeaf = Leaf

-- | Creates a pure tree component from a function
pureTree ::
  forall props. Lacks "children" props
  => (Record props -> Children -> Element)
  -> Tree props
pureTree c = Tree $ \props -> c (unsafeCoerce props) props.children

-- | Creates a hooks leaf component from a function
hooksLeaf ::
  forall props. Lacks "children" props
  => (Record props -> Hooks Element)
  -> Leaf props
hooksLeaf c = pureLeaf (runHooks <<< c)

hooksTree ::
  forall props. Lacks "children" props
  => (Record props -> Children -> Hooks Element)
  -> Tree props
hooksTree c = Tree $ \props -> runHooks $ c (unsafeCoerce props) props.children





-- element creation

-- | Creates a leaf component from a props Record
createLeaf ::
  forall tree props. Lacks "children" props
  => LeafComponent tree props
  => tree
  -> Record props
  -> Element
createLeaf c p = runFn3 _createElement c p []

-- | Creates a tree component from a props Record and an Array of children
createTree ::
  forall tree props. Lacks "children" props
  => TreeComponent tree props
  => tree
  -> Record props
  -> Array Element
  -> Element
createTree = runFn3 _createElement

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

memoLeaf ::
  forall props. Lacks "children" props
  => Leaf props
  -> (Record props -> Record props -> Boolean)
  -> MemoLeaf props
memoLeaf (Leaf l) = runFn2 _memo l

memoLeaf' ::
  forall props. Lacks "children" props
  => Leaf props
  -> MemoLeaf props
memoLeaf' (Leaf l) = _memoPrime l

memoTree ::
  forall props. Lacks "children" props
  => Tree props
  -> (Record (WithChildren props) -> Record (WithChildren props) -> Boolean)
  -> MemoTree props
memoTree (Tree t) = runFn2 _memo t

memoTree' ::
  forall props. Lacks "children" props
  => Tree props
  -> MemoTree props
memoTree' (Tree t) = _memoPrime t

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


