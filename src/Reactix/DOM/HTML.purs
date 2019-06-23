module Reactix.DOM.HTML where

import Reactix.React (Element, createDOMElement)
import Unsafe.Coerce (unsafeCoerce)

createLeafElement :: forall props. String -> Record props -> Element
createLeafElement e p = createDOMElement e p []

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
a = createDOMElement "a"

abbr :: ElemFactory
abbr = createDOMElement "abbr"

address :: ElemFactory
address = createDOMElement "address"

article :: ElemFactory
article = createDOMElement "article"

aside :: ElemFactory
aside = createDOMElement "aside"

audio :: ElemFactory
audio = createDOMElement "audio"

b :: ElemFactory
b = createDOMElement "b"

bdi :: ElemFactory
bdi = createDOMElement "bdi"

bdo :: ElemFactory
bdo = createDOMElement "bdo"

blockquote :: ElemFactory
blockquote = createDOMElement "blockquote"

br :: LeafFactory
br = createLeafElement "br"

button :: ElemFactory
button = createDOMElement "button"

canvas :: ElemFactory
canvas = createDOMElement "canvas"

caption :: ElemFactory
caption = createDOMElement "caption"

cite :: ElemFactory
cite = createDOMElement "cite"

code :: ElemFactory
code = createDOMElement "code"

col :: LeafFactory
col = createLeafElement "col"

colgroup :: ElemFactory
colgroup = createDOMElement "colgroup"

data' :: ElemFactory
data' = createDOMElement "data"

datalist :: ElemFactory
datalist = createDOMElement "datalist"

dd :: ElemFactory
dd = createDOMElement "dd"

del :: ElemFactory
del = createDOMElement "del"

details :: ElemFactory
details = createDOMElement "details"

dfn :: ElemFactory
dfn = createDOMElement "dfn"

dialog :: ElemFactory
dialog = createDOMElement "dialog"

div :: ElemFactory
div = createDOMElement "div"

dl :: ElemFactory
dl = createDOMElement "dl"

dt :: ElemFactory
dt = createDOMElement "dt"

em :: ElemFactory
em = createDOMElement "em"

embed :: LeafFactory
embed = createLeafElement "embed"

fieldset :: ElemFactory
fieldset = createDOMElement "fieldset"

figcaption :: ElemFactory
figcaption = createDOMElement "figcaption"

figure :: ElemFactory
figure = createDOMElement "figure"

footer :: ElemFactory
footer = createDOMElement "footer"

form :: ElemFactory
form = createDOMElement "form"

h1 :: ElemFactory
h1 = createDOMElement "h1"

h2 :: ElemFactory
h2 = createDOMElement "h2"

h3 :: ElemFactory
h3 = createDOMElement "h3"

h4 :: ElemFactory
h4 = createDOMElement "h4"

h5 :: ElemFactory
h5 = createDOMElement "h5"

h6 :: ElemFactory
h6 = createDOMElement "h6"

header :: ElemFactory
header = createDOMElement "header"

hr :: LeafFactory
hr = createLeafElement "hr"

i :: ElemFactory
i = createDOMElement "i"

iframe :: ElemFactory
iframe = createDOMElement "iframe"

img :: LeafFactory
img = createLeafElement "img"

input :: LeafFactory
input = createLeafElement "input"

ins :: ElemFactory
ins = createDOMElement "ins"

kbd :: ElemFactory
kbd = createDOMElement "kbd"

label :: ElemFactory
label = createDOMElement "label"

legend :: ElemFactory
legend = createDOMElement "legend"

li :: ElemFactory
li = createDOMElement "li"

link :: LeafFactory
link = createLeafElement "link"

main :: ElemFactory
main = createDOMElement "main"

mark :: ElemFactory
mark = createDOMElement "mark"

meter :: ElemFactory
meter = createDOMElement "meter"

nav :: ElemFactory
nav = createDOMElement "nav"

object :: ElemFactory
object = createDOMElement "object"

ol :: ElemFactory
ol = createDOMElement "ol"

optgroup :: ElemFactory
optgroup = createDOMElement "optgroup"

option :: ElemFactory
option = createDOMElement "option"

output :: ElemFactory
output = createDOMElement "output"

p :: ElemFactory
p = createDOMElement "p"

param :: LeafFactory
param = createLeafElement "param"

picture :: ElemFactory
picture = createDOMElement "picture"

pre :: ElemFactory
pre = createDOMElement "pre"

progress :: ElemFactory
progress = createDOMElement "progress"

q :: ElemFactory
q = createDOMElement "q"

rp :: ElemFactory
rp = createDOMElement "rp"

rt :: ElemFactory
rt = createDOMElement "rt"

ruby :: ElemFactory
ruby = createDOMElement "ruby"

s :: ElemFactory
s = createDOMElement "s"

samp :: ElemFactory
samp = createDOMElement "samp"

section :: ElemFactory
section = createDOMElement "section"

source :: LeafFactory
source = createLeafElement "source"

span :: ElemFactory
span = createDOMElement "span"

strong :: ElemFactory
strong = createDOMElement "strong"

style :: ElemFactory
style = createDOMElement "style"

sub :: ElemFactory
sub = createDOMElement "sub"

sup :: ElemFactory
sup = createDOMElement "sup"

summary :: ElemFactory
summary = createDOMElement "summary"

svg :: ElemFactory
svg = createDOMElement "svg"

table :: ElemFactory 
table = createDOMElement "table"

tbody :: ElemFactory
tbody = createDOMElement "tbody"

td :: ElemFactory
td = createDOMElement "td"

template :: ElemFactory
template = createDOMElement "template"

textarea :: ElemFactory 
textarea = createDOMElement "textarea"

tfoot :: ElemFactory
tfoot = createDOMElement "tfoot"

th :: ElemFactory 
th = createDOMElement "th"

thead :: ElemFactory 
thead = createDOMElement "thead"

time :: ElemFactory
time = createDOMElement "time"

title :: ElemFactory 
title = createDOMElement "title"

tr :: ElemFactory 
tr = createDOMElement "tr"

track :: LeafFactory
track = createLeafElement "track"

u :: ElemFactory
u = createDOMElement "u"

ul :: ElemFactory
ul = createDOMElement "ul"

var :: ElemFactory
var = createDOMElement "var"

video :: ElemFactory
video = createDOMElement "video"

wbr :: LeafFactory
wbr = createLeafElement "wbr"
