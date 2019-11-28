module Reactix.DOM.HTML where

import Prelude (identity, ($), (<<<), (<>))
import Data.Maybe (maybe)
import Data.Foldable (foldl)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple.Objects ((.?), (.=), (!-), (..), keys)
import Reactix.React (Element, createDOMElement)
import Reactix.Utils (ucFirst)
import Unsafe.Coerce (unsafeCoerce)

createDOM :: forall props. String -> Record props -> Array Element -> Element
createDOM e props = createDOMElement e (magicProps props)

magicProps :: forall props. props -> props
magicProps = xformAriaProps <<< xformDataProps <<< xformEventProps
  where
    xformAriaProps = magicPrefixProp "aria" "aria-"
    xformDataProps = magicPrefixProp "data" "data-"

magicPrefixProp :: forall props. String -> String -> props -> props
magicPrefixProp prop pre' props = maybe props help (props .? prop)
  where help val = prefixCopyAll pre' props val !- prop

xformEventProps :: forall props. props -> props
xformEventProps props = maybe props help (props .? "on")
  where
    help = mapCopyAll eventPropName mkEffectFn1 props
    eventPropName other = "on" <> ucFirst other

prefixCopyAll :: forall p q. String -> p -> q -> p
prefixCopyAll pre' = mapCopyAll (pre' <> _) identity

mapCopyAll :: forall a b p q. (String -> String) -> (a -> b) -> p -> q -> p
mapCopyAll xf yf dest src = foldl f dest (keys src)
  where f dest' k = (dest' .= xf k) (yf $ src .. k)

createLeafDOM :: forall props. String -> Record props -> Element
createLeafDOM e props = createDOM e props []

-- A factory function for a DOM element with no children
type LeafFactory = forall props. Record props -> Element

-- A factory function for a DOM element with children
type ElemFactory = forall props. Record props -> Array Element -> Element

-- type GlobalProps =
--   ( accessKey :: String
--   , className :: String
--   , contentEditable :: Boolean
--   , dir :: String
--   , draggable :: Boolean
--   , dropzone :: String
--   , hidden :: String
--   , id :: String
--   , lang :: String
--   , spellcheck :: Boolean
--   , style :: (forall r. Record r)
--   , tabIndex :: Int
--   , title :: String
--   , translate :: String )

text :: String -> Element
text = unsafeCoerce

a :: ElemFactory
a = createDOM "a"

abbr :: ElemFactory
abbr = createDOM "abbr"

address :: ElemFactory
address = createDOM "address"

article :: ElemFactory
article = createDOM "article"

aside :: ElemFactory
aside = createDOM "aside"

audio :: ElemFactory
audio = createDOM "audio"

b :: ElemFactory
b = createDOM "b"

bdi :: ElemFactory
bdi = createDOM "bdi"

bdo :: ElemFactory
bdo = createDOM "bdo"

blockquote :: ElemFactory
blockquote = createDOM "blockquote"

br :: LeafFactory
br = createLeafDOM "br"

button :: ElemFactory
button = createDOM "button"

canvas :: ElemFactory
canvas = createDOM "canvas"

caption :: ElemFactory
caption = createDOM "caption"

cite :: ElemFactory
cite = createDOM "cite"

code :: ElemFactory
code = createDOM "code"

col :: LeafFactory
col = createLeafDOM "col"

colgroup :: ElemFactory
colgroup = createDOM "colgroup"

data' :: ElemFactory
data' = createDOM "data"

datalist :: ElemFactory
datalist = createDOM "datalist"

dd :: ElemFactory
dd = createDOM "dd"

del :: ElemFactory
del = createDOM "del"

details :: ElemFactory
details = createDOM "details"

dfn :: ElemFactory
dfn = createDOM "dfn"

dialog :: ElemFactory
dialog = createDOM "dialog"

div :: ElemFactory
div = createDOM "div"

dl :: ElemFactory
dl = createDOM "dl"

dt :: ElemFactory
dt = createDOM "dt"

em :: ElemFactory
em = createDOM "em"

embed :: LeafFactory
embed = createLeafDOM "embed"

fieldset :: ElemFactory
fieldset = createDOM "fieldset"

figcaption :: ElemFactory
figcaption = createDOM "figcaption"

figure :: ElemFactory
figure = createDOM "figure"

footer :: ElemFactory
footer = createDOM "footer"

form :: ElemFactory
form = createDOM "form"

h1 :: ElemFactory
h1 = createDOM "h1"

h2 :: ElemFactory
h2 = createDOM "h2"

h3 :: ElemFactory
h3 = createDOM "h3"

h4 :: ElemFactory
h4 = createDOM "h4"

h5 :: ElemFactory
h5 = createDOM "h5"

h6 :: ElemFactory
h6 = createDOM "h6"

header :: ElemFactory
header = createDOM "header"

hr :: LeafFactory
hr = createLeafDOM "hr"

i :: ElemFactory
i = createDOM "i"

iframe :: ElemFactory
iframe = createDOM "iframe"

img :: LeafFactory
img = createLeafDOM "img"

input :: LeafFactory
input = createLeafDOM "input"

ins :: ElemFactory
ins = createDOM "ins"

kbd :: ElemFactory
kbd = createDOM "kbd"

label :: ElemFactory
label = createDOM "label"

legend :: ElemFactory
legend = createDOM "legend"

li :: ElemFactory
li = createDOM "li"

link :: LeafFactory
link = createLeafDOM "link"

main :: ElemFactory
main = createDOM "main"

mark :: ElemFactory
mark = createDOM "mark"

meter :: ElemFactory
meter = createDOM "meter"

nav :: ElemFactory
nav = createDOM "nav"

object :: ElemFactory
object = createDOM "object"

ol :: ElemFactory
ol = createDOM "ol"

optgroup :: ElemFactory
optgroup = createDOM "optgroup"

option :: ElemFactory
option = createDOM "option"

output :: ElemFactory
output = createDOM "output"

p :: ElemFactory
p = createDOM "p"

param :: LeafFactory
param = createLeafDOM "param"

picture :: ElemFactory
picture = createDOM "picture"

pre :: ElemFactory
pre = createDOM "pre"

progress :: ElemFactory
progress = createDOM "progress"

q :: ElemFactory
q = createDOM "q"

rp :: ElemFactory
rp = createDOM "rp"

rt :: ElemFactory
rt = createDOM "rt"

ruby :: ElemFactory
ruby = createDOM "ruby"

s :: ElemFactory
s = createDOM "s"

samp :: ElemFactory
samp = createDOM "samp"

section :: ElemFactory
section = createDOM "section"

source :: LeafFactory
source = createLeafDOM "source"

span :: ElemFactory
span = createDOM "span"

strong :: ElemFactory
strong = createDOM "strong"

style :: ElemFactory
style = createDOM "style"

sub :: ElemFactory
sub = createDOM "sub"

sup :: ElemFactory
sup = createDOM "sup"

summary :: ElemFactory
summary = createDOM "summary"

svg :: ElemFactory
svg = createDOM "svg"

table :: ElemFactory 
table = createDOM "table"

tbody :: ElemFactory
tbody = createDOM "tbody"

td :: ElemFactory
td = createDOM "td"

template :: ElemFactory
template = createDOM "template"

textarea :: ElemFactory 
textarea = createDOM "textarea"

tfoot :: ElemFactory
tfoot = createDOM "tfoot"

th :: ElemFactory 
th = createDOM "th"

thead :: ElemFactory 
thead = createDOM "thead"

time :: ElemFactory
time = createDOM "time"

title :: ElemFactory 
title = createDOM "title"

tr :: ElemFactory 
tr = createDOM "tr"

track :: LeafFactory
track = createLeafDOM "track"

u :: ElemFactory
u = createDOM "u"

ul :: ElemFactory
ul = createDOM "ul"

var :: ElemFactory
var = createDOM "var"

video :: ElemFactory
video = createDOM "video"

wbr :: LeafFactory
wbr = createLeafDOM "wbr"
