module Reactix.Test
  ( act
  , render
  , Rendered
  , fireEvent, fireClick
  ) where

import Prelude ( Unit )
import Effect ( Effect )
import Effect.Uncurried ( EffectFn1, runEffectFn1, EffectFn2, runEffectFn2 )
import Data.Function.Uncurried ( Fn2, runFn2 )
import DOM.Simple as DOM
import Reactix.React ( Element )

type Rendered =
  { getByText :: EffectFn1 String Element
  , getByTestId :: EffectFn1 String Element
  , container :: DOM.Element
  , asFragment :: Effect DOM.Fragment }

render :: Element -> Effect Rendered
render = runEffectFn1 _render

foreign import _render :: EffectFn1 Element Rendered

-- | Make react behave more predictably in tests
act :: forall t. Effect t -> Effect t
act = runEffectFn1 _act

foreign import _act :: forall t. EffectFn1 (Effect t) t

fireClick :: DOM.Element -> Effect Unit
fireClick = fireEvent "click"

fireEvent :: String -> DOM.Element -> Effect Unit
fireEvent = runEffectFn2 _fireEvent

foreign import _fireEvent :: EffectFn2 String DOM.Element Unit
