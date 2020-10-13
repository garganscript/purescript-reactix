-- | https://reactjs.org/docs/events.html
module Reactix.SyntheticEvent where

import Prelude
import DOM.Simple as DOM
import DOM.Simple.Event (class HasModifierKeys, class IsEvent, class IsMouseEvent, KeyboardEvent, MouseButtonEvent)
import Effect ( Effect )
import FFI.Simple ( (..), (...), delay )
import Unsafe.Coerce (unsafeCoerce)

foreign import data SyntheticEvent :: Type -> Type

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

unsafeEventTarget :: forall event. event -> DOM.Element
unsafeEventTarget e = (unsafeCoerce e).target

bubbles :: forall e. IsEvent e => SyntheticEvent e -> Boolean
bubbles e = e .. "bubbles"

cancelable :: forall e. IsEvent e => SyntheticEvent e -> Boolean
cancelable e = e .. "cancelable"

isTrusted :: forall e. IsEvent e => SyntheticEvent e -> Boolean
isTrusted e = e .. "isTrusted"

defaultPrevented :: forall e. IsEvent e => SyntheticEvent e -> Boolean
defaultPrevented e = e .. "defaultPrevented"

eventPhase :: forall e. IsEvent e => SyntheticEvent e -> Number
eventPhase e = e .. "eventPhase"

timestamp :: forall e. IsEvent e => SyntheticEvent e -> Number
timestamp e = e .. "timeStamp"

type' :: forall e. IsEvent e => SyntheticEvent e -> String
type' e = e .. "type"

target :: forall e. IsEvent e => SyntheticEvent e -> DOM.Element
target e = e .. "target"

currentTarget :: forall e. IsEvent e => SyntheticEvent e -> DOM.Element
currentTarget e = e .. "currentTarget"

nativeEvent :: forall e. IsEvent e => SyntheticEvent e -> e
nativeEvent e = e .. "nativeEvent"

stopPropagation :: forall e. IsEvent e => SyntheticEvent e -> Effect Unit
stopPropagation e = delay unit $ \_ -> pure $ e ... "stopPropagation" $ []

preventDefault :: forall e. IsEvent e => SyntheticEvent e -> Effect Unit
preventDefault e = delay unit $ \_ -> pure $ e ... "preventDefault" $ []

isPropagationStopped :: forall e. IsEvent e => SyntheticEvent e -> Effect Unit
isPropagationStopped e = e ... "isPropagationStopped" $ []

isDefaultPrevented :: forall e. IsEvent e => SyntheticEvent e -> Effect Unit
isDefaultPrevented e = e ... "isDefaultPrevented" $ []

-- Events with Modifier keys

altKey :: forall e. HasModifierKeys e => SyntheticEvent e -> Boolean
altKey e = e .. "altKey"
ctrlKey :: forall e. HasModifierKeys e => SyntheticEvent e -> Boolean
ctrlKey e = e .. "ctrlKey"
shiftKey :: forall e. HasModifierKeys e => SyntheticEvent e -> Boolean
shiftKey e = e .. "shiftKey"
metaKey :: forall e. HasModifierKeys e => SyntheticEvent e -> Boolean
metaKey e = e .. "metaKey"
getModifierState :: forall e. HasModifierKeys e => SyntheticEvent e -> String -> Boolean
getModifierState e s = e ... "getModifierState" $ [ s ]

-- Keyboard Events

key :: SyntheticEvent KeyboardEvent -> String
key e = e .. "key"
which :: SyntheticEvent KeyboardEvent -> Number
which e = e .. "which"
charCode :: SyntheticEvent KeyboardEvent -> Int
charCode e = e .. "charCode"
keyCode :: SyntheticEvent KeyboardEvent -> Number
keyCode e = e .. "keyCode"
locale :: SyntheticEvent KeyboardEvent -> String
locale e = e .. "locale"
location :: SyntheticEvent KeyboardEvent -> Number
location e = e .. "location"
repeat :: SyntheticEvent KeyboardEvent -> Boolean
repeat e = e .. "repeat"

button :: SyntheticEvent MouseButtonEvent -> Number
button e = e .. "button"
buttons :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
buttons e = e .. "buttons"
relatedTarget :: forall e. IsMouseEvent e => SyntheticEvent e -> DOM.Element
relatedTarget e = e .. "relatedTarget"
clientX :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
clientX e = e .. "clientX"
clientY :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
clientY e = e .. "clientY"
pageX :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
pageX e = e .. "pageX"
pageY :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
pageY e = e .. "pageY"
screenX :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
screenX e = e .. "screenX"
screenY :: forall e. IsMouseEvent e => SyntheticEvent e -> Number
screenY e = e .. "screenY"

-- changedTouches :: TouchEvent -> TouchList
-- targetTouches :: TouchEvent -> TouchList
-- touches :: TouchEvent -> TouchList

-- foreign import data AnimationEvent :: Type
-- animationName :: AnimationEvent -> String
-- pseudoElement :: AnimationEvent -> String
-- elapsedTime :: AnimationEvent -> Number

-- foreign import data ClipboardEvent :: Type
-- clipboardData :: ClipboardEvent -> DataTransfer

-- foreign import data CompositionEvent :: Type
-- data' :: SyntheticCompositionEvent -> String

-- foreign import data FocusEvent :: Type
-- relatedTarget :: FocusEvent -> DOM.Element

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


