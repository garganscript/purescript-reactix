module Reactix.DOM.HTML where

import Reactix.React (Element, createElement)
import Unsafe.Coerce (unsafeCoerce)

createLeafElement :: forall props. String -> Record props -> Element
createLeafElement e p = createElement e p []

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
a = createElement "a"

abbr :: ElemFactory
abbr = createElement "abbr"

address :: ElemFactory
address = createElement "address"

article :: ElemFactory
article = createElement "article"

aside :: ElemFactory
aside = createElement "aside"

audio :: ElemFactory
audio = createElement "audio"

b :: ElemFactory
b = createElement "b"

bdi :: ElemFactory
bdi = createElement "bdi"

bdo :: ElemFactory
bdo = createElement "bdo"

blockquote :: ElemFactory
blockquote = createElement "blockquote"

br :: LeafFactory
br = createLeafElement "br"

button :: ElemFactory
button = createElement "button"

canvas :: ElemFactory
canvas = createElement "canvas"

caption :: ElemFactory
caption = createElement "caption"

cite :: ElemFactory
cite = createElement "cite"

code :: ElemFactory
code = createElement "code"

col :: LeafFactory
col = createLeafElement "col"

colgroup :: ElemFactory
colgroup = createElement "colgroup"

data' :: ElemFactory
data' = createElement "data"

datalist :: ElemFactory
datalist = createElement "datalist"

dd :: ElemFactory
dd = createElement "dd"

del :: ElemFactory
del = createElement "del"

details :: ElemFactory
details = createElement "details"

dfn :: ElemFactory
dfn = createElement "dfn"

dialog :: ElemFactory
dialog = createElement "dialog"

div :: ElemFactory
div = createElement "div"

dl :: ElemFactory
dl = createElement "dl"

dt :: ElemFactory
dt = createElement "dt"

em :: ElemFactory
em = createElement "em"

embed :: LeafFactory
embed = createLeafElement "embed"

fieldset :: ElemFactory
fieldset = createElement "fieldset"

figcaption :: ElemFactory
figcaption = createElement "figcaption"

figure :: ElemFactory
figure = createElement "figure"

footer :: ElemFactory
footer = createElement "footer"

form :: ElemFactory
form = createElement "form"

h1 :: ElemFactory
h1 = createElement "h1"

h2 :: ElemFactory
h2 = createElement "h2"

h3 :: ElemFactory
h3 = createElement "h3"

h4 :: ElemFactory
h4 = createElement "h4"

h5 :: ElemFactory
h5 = createElement "h5"

h6 :: ElemFactory
h6 = createElement "h6"

header :: ElemFactory
header = createElement "header"

hr :: LeafFactory
hr = createLeafElement "hr"

i :: ElemFactory
i = createElement "i"

iframe :: ElemFactory
iframe = createElement "iframe"

img :: LeafFactory
img = createLeafElement "img"

input :: LeafFactory
input = createLeafElement "input"

ins :: ElemFactory
ins = createElement "ins"

kbd :: ElemFactory
kbd = createElement "kbd"

label :: ElemFactory
label = createElement "label"

legend :: ElemFactory
legend = createElement "legend"

li :: ElemFactory
li = createElement "li"

link :: LeafFactory
link = createLeafElement "link"

main :: ElemFactory
main = createElement "main"

mark :: ElemFactory
mark = createElement "mark"

meter :: ElemFactory
meter = createElement "meter"

nav :: ElemFactory
nav = createElement "nav"

object :: ElemFactory
object = createElement "object"

ol :: ElemFactory
ol = createElement "ol"

optgroup :: ElemFactory
optgroup = createElement "optgroup"

option :: ElemFactory
option = createElement "option"

output :: ElemFactory
output = createElement "output"

p :: ElemFactory
p = createElement "p"

param :: LeafFactory
param = createLeafElement "param"

picture :: ElemFactory
picture = createElement "picture"

pre :: ElemFactory
pre = createElement "pre"

progress :: ElemFactory
progress = createElement "progress"

q :: ElemFactory
q = createElement "q"

rp :: ElemFactory
rp = createElement "rp"

rt :: ElemFactory
rt = createElement "rt"

ruby :: ElemFactory
ruby = createElement "ruby"

s :: ElemFactory
s = createElement "s"

samp :: ElemFactory
samp = createElement "samp"

section :: ElemFactory
section = createElement "section"

source :: LeafFactory
source = createLeafElement "source"

span :: ElemFactory
span = createElement "span"

strong :: ElemFactory
strong = createElement "strong"

style :: ElemFactory
style = createElement "style"

sub :: ElemFactory
sub = createElement "sub"

sup :: ElemFactory
sup = createElement "sup"

summary :: ElemFactory
summary = createElement "summary"

svg :: ElemFactory
svg = createElement "svg"

table :: ElemFactory 
table = createElement "table"

tbody :: ElemFactory
tbody = createElement "tbody"

td :: ElemFactory
td = createElement "td"

template :: ElemFactory
template = createElement "template"

textarea :: ElemFactory 
textarea = createElement "textarea"

tfoot :: ElemFactory
tfoot = createElement "tfoot"

th :: ElemFactory 
th = createElement "th"

thead :: ElemFactory 
thead = createElement "thead"

time :: ElemFactory
time = createElement "time"

title :: ElemFactory 
title = createElement "title"

tr :: ElemFactory 
tr = createElement "tr"

track :: LeafFactory
track = createLeafElement "track"

u :: ElemFactory
u = createElement "u"

ul :: ElemFactory
ul = createElement "ul"

var :: ElemFactory
var = createElement "var"

video :: ElemFactory
video = createElement "video"

wbr :: LeafFactory
wbr = createLeafElement "wbr"
