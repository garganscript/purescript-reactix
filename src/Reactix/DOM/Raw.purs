module Reactix.DOM.Raw
  (LeafFactory, ElementFactory
  , button, div, div', hr
  , i, i', p, p', span, span'
  , text) where

import Reactix.React (Element, createElement)
import Unsafe.Coerce (unsafeCoerce)

createLeafElement :: forall props. String -> Record props -> Element
createLeafElement e p = createElement e p []

-- A factory function for a DOM element with no children
type LeafFactory = forall props. Record props -> Element

-- A factory function for a DOM element with children
type ElementFactory = forall props. Record props -> Array Element -> Element

text :: String -> Element
text = unsafeCoerce

button :: ElementFactory
button = createElement "button"

div :: ElementFactory
div = createElement "div"

div' :: LeafFactory
div' = createLeafElement "div"

hr :: LeafFactory
hr = createLeafElement "hr"

i :: ElementFactory
i = createElement "i"

i' :: LeafFactory
i' = createLeafElement "i"

p :: ElementFactory
p = createElement "p"

p' :: LeafFactory
p' = createLeafElement "p"

span :: ElementFactory
span = createElement "span"

span' :: LeafFactory
span' = createLeafElement "span"

