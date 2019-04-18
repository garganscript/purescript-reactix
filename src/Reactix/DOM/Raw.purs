module Reactix.DOM.Raw
  (LeafFactory, TreeFactory
  , div, div', i, i', button
  , text) where

import Reactix.React (Element, createDOMElement)
import Unsafe.Coerce (unsafeCoerce)

createLeafDOMElement :: forall props. String -> Record props -> Element
createLeafDOMElement e p = createDOMElement e p []

-- A factory function for a DOM element with no children
type LeafFactory = forall props. Record props -> Element

-- A factory function for a DOM element with children
type TreeFactory = forall props. Record props -> Array Element -> Element

button :: TreeFactory
button = createDOMElement "button"

div :: TreeFactory
div = createDOMElement "div"

div' :: LeafFactory
div' = createLeafDOMElement "div"

i :: TreeFactory
i = createDOMElement "i"

i' :: LeafFactory
i' = createLeafDOMElement "i"

text :: String -> Element
text = unsafeCoerce

