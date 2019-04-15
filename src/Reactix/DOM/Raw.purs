module Reactix.DOM.Raw (text) where

import Reactix.React (Element)
import Unsafe.Coerce (unsafeCoerce)

text :: String -> Element
text = unsafeCoerce

