module Reactix.DOM.Raw
  (LeafFactory, TreeFactory
  , button, div, div', i, i', p, p', span, span'
  , text) where

import Reactix.React (Element, createDOMElement)
import Unsafe.Coerce (unsafeCoerce)

createLeafDOMElement :: forall props. String -> Record props -> Element
createLeafDOMElement e p = createDOMElement e p []

-- A factory function for a DOM element with no children
type LeafFactory = forall props. Record props -> Element

-- A factory function for a DOM element with children
type TreeFactory = forall props. Record props -> Array Element -> Element

text :: String -> Element
text = unsafeCoerce

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

p :: TreeFactory
p = createDOMElement "p"

p' :: LeafFactory
p' = createLeafDOMElement "p"

span :: TreeFactory
span = createDOMElement "span"

span' :: LeafFactory
span' = createLeafDOMElement "span"

