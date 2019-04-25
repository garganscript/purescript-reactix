-- | https://reactjs.org/docs/events.html
module Reactix.SyntheticEvent where

import Prelude
import DOM.Simple as DOM
import Effect ( Effect )
import Effect.Uncurried ( EffectFn1, runEffectFn1 )
import FFI.Simple ( (..), (...) )

class IsSyntheticEvent e

foreign import data NativeEvent :: Type

foreign import data MouseEvent :: Type
foreign import data KeyboardEvent :: Type

instance keyboardEventIsSyntheticEvent :: IsSyntheticEvent KeyboardEvent
instance mouseEventIsSyntheticEvent :: IsSyntheticEvent MouseEvent

bubbles :: forall e. IsSyntheticEvent e => e -> Boolean
bubbles e = e .. "bubbles"

cancelable :: forall e. IsSyntheticEvent e => e -> Boolean
cancelable e = e .. "cancelable"

isTrusted :: forall e. IsSyntheticEvent e => e -> Boolean
isTrusted e = e .. "isTrusted"

defaultPrevented :: forall e. IsSyntheticEvent e => e -> Boolean
defaultPrevented e = e .. "defaultPrevented"

eventPhase :: forall e. IsSyntheticEvent e => e -> Number
eventPhase e = e .. "eventPhase"

timestamp :: forall e. IsSyntheticEvent e => e -> Number
timestamp e = e .. "timeStamp"

type' :: forall e. IsSyntheticEvent e => e -> String
type' e = e .. "type"

-- target :: forall e. IsSyntheticEvent e => e -> NativeEventTarget
-- target e = e .. "target"

-- currentTarget :: forall e. IsSyntheticEvent e => e -> NativeEventTarget
-- currentTarget e = e .. "currentTarget"

-- nativeEvent :: forall e. IsSyntheticEvent e => e -> NativeEvent
-- nativeEvent e = e .. "nativeEvent"

stopPropagation :: forall e. IsSyntheticEvent e => e -> Effect Unit
stopPropagation e = e ... "stopPropagation" $ []

preventDefault :: forall e. IsSyntheticEvent e => e -> Effect Unit
preventDefault e = e ... "preventDefault" $ []

isPropagationStopped :: forall e. IsSyntheticEvent e => e -> Effect Unit
isPropagationStopped e = e ... "isPropagationStopped" $ []

isDefaultPrevented :: forall e. IsSyntheticEvent e => e -> Effect Unit
isDefaultPrevented e = e ... "isDefaultPrevented" $ []

-- Events with Modifier keys

-- | This class is used to access information about modifier keys for
-- | supported events
class HasModifierKeys e

instance mouseEventHasModifierKeys :: HasModifierKeys MouseEvent
instance keyboardEventHasModifierKeys :: HasModifierKeys KeyboardEvent
-- instance touchEventHasModifierKeys :: HasModifierKeys TouchEvent

altKey :: forall e. HasModifierKeys e => e -> Boolean
altKey e = e .. "altKey"
ctrlKey :: forall e. HasModifierKeys e => e -> Boolean
ctrlKey e = e .. "ctrlKey"
shiftKey :: forall e. HasModifierKeys e => e -> Boolean
shiftKey e = e .. "shiftKey"
metaKey :: forall e. HasModifierKeys e => e -> Boolean
metaKey e = e .. "metaKey"
getModifierState :: forall e. HasModifierKeys e => e -> String -> Boolean
getModifierState e s = e ... "getModifierState" $ [ s ]

-- Keyboard Events

key :: KeyboardEvent -> String
key e = e .. "key"
which :: KeyboardEvent -> Number
which e = e .. "which"
charCode :: KeyboardEvent -> Int
charCode e = e .. "charCode"
keyCode :: KeyboardEvent -> Number
keyCode e = e .. "keyCode"
locale :: KeyboardEvent -> String
locale e = e .. "locale"
location :: KeyboardEvent -> Number
location e = e .. "location"
repeat :: KeyboardEvent -> Boolean
repeat e = e .. "repeat"

button :: MouseEvent -> Number
button e = e .. "button"
buttons :: MouseEvent -> Number
buttons e = e .. "buttons"
-- relatedTarget :: MouseEvent -> NativeEventTarget
-- relatedTarget e = e .. "relatedTarget"
clientX :: MouseEvent -> Number
clientX e = e .. "clientX"
clientY :: MouseEvent -> Number
clientY e = e .. "clientY"
pageX :: MouseEvent -> Number
pageX e = e .. "pageX"
pageY :: MouseEvent -> Number
pageY e = e .. "pageY"
screenX :: MouseEvent -> Number
screenX e = e .. "screenX"
screenY :: MouseEvent -> Number
screenY e = e .. "screenY"

-- foreign import data TouchEvent :: Type

-- changedTouches :: TouchEvent -> NativeTouchList
-- targetTouches :: TouchEvent -> NativeTouchList
-- touches :: TouchEvent -> NativeTouchList


-- foreign import data NativeDataTransfer :: Type
-- foreign import data NativeAbstractView :: Type
-- foreign import data NativeTouchList :: Type

-- foreign import data AnimationEvent :: Type
-- animationName :: AnimationEvent -> String
-- pseudoElement :: AnimationEvent -> String
-- elapsedTime :: AnimationEvent -> Number

-- foreign import data ClipboardEvent :: Type
-- clipboardData :: ClipboardEvent -> NativeDataTransfer

-- foreign import data CompositionEvent :: Type
-- data' :: SyntheticCompositionEvent -> String

-- foreign import data FocusEvent :: Type
-- relatedTarget :: FocusEvent -> NativeEventTarget


-- foreign import data TransitionEvent :: Type
-- propertyName :: TransitionEvent -> String
-- pseudoElement :: TransitionEvent -> String
-- elapsedTime :: TransitionEvent -> Number

-- foreign import data UIEvent :: Type
-- detail :: UIEvent -> Number
-- view :: UIEvent -> NativeAbstractView

-- foreign import data WheelEvent :: Type
-- deltaMode :: WheelEvent -> Number
-- deltaX :: WheelEvent -> Number
-- deltaY :: WheelEvent -> Number
-- deltaZ :: WheelEvent -> Number


