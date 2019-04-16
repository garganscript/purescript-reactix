module Reactix.Mount ( findElementBySelector, render ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class ( class MonadEffect, liftEffect )
import Effect.Uncurried ( EffectFn1, runEffectFn1, EffectFn2, runEffectFn2 )
import Reactix.React ( class MonadHooks, Element, DOMElement)

-- findElementBySelector

-- | Finds a root in the document to which we can render a component
findElementBySelector :: String -> Effect (Maybe DOMElement)
findElementBySelector i = toMaybe <$> runEffectFn1 _getElementBySelector i

foreign import _getElementBySelector :: EffectFn1 String (Nullable DOMElement)

-- renderToRoot

foreign import _render :: EffectFn2 Element DOMElement Unit

-- | Renders a React Element to a real Element
render :: forall m. MonadEffect m => MonadHooks m => Element -> DOMElement -> m Unit
render e d = liftEffect (runEffectFn2 _render e d)
