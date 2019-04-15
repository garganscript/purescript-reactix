# Tutorial

## You know HTML, right?

Here's some really simple HTML demonstrating the horizontal rule (hr)
element in action:

```html
<hr width="80%" height="1" />
```

In the html model we could say:

- We are creating a `hr` element
- `hr` is the type of the element
- `width` and `height` are the attributes to construct it with
- The element will have no children 

In the React model, we would say:

- We are creating a React element
- `hr` is the is the constructor of the component
- `width` and `height` are the props to construct it with
- The element will have no children

One way of doing that with this library is this:

```purescript
import Reactix (Element)
import Reactix.DOM.Raw (hr)

myHR :: Element
myHR = hr {width: "80%", height: 1}
```

Here is a complete Reactix program that renders this `Element` to the
`<body>` of the page it is running in:

```purescript
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Reactix (Element, DOMElement, findElementBySelector, render)
import Reactix.DOM.Raw (hr)

myHR :: Element
myHR = hr {width: "80%", height: 1}

main :: Effect Unit
main = do r <- findElementBySelector "body" -- find <body>
          tryMount r
  where tryMount :: Maybe Root -> Effect Unit
        tryMount (Just root) = render myHR root
        tryMount Nothing = log "Could not find body"
```

## Trees and Leaves

We know from HTML that some Elements accept children and some do
not. This is also the case with React components. Reactix makes it
explicit in the type whether a function accepts children or not.

We say that a component which accepts children is a `tree component`
and a component which does not is a `leaf component`. Our `hr` example
is a `leaf component` because it does not accept children. We have not
yet seen a `tree component`, but `a` is an example.

Let's look at an `a` now:

```html
<a href="#">Hello World</a>
```

In Reactix:

```purescript
import Reactix (Element)
import Reactix.DOM.Raw (a, text)

helloWorld :: Element
helloWorld = a {href: "#"} [ text "Hello World" ]
```

Here's a slightly more complicated example:

```html
<a href="#"><b>Hello</b> World</a>
```

In Reactix:

```purescript
import Reactix (Element)
import Reactix.DOM.Raw (a, b text)
`
helloWorld :: Element
helloWorld = a {href: "#"} [ hello, world ]
  where hello = b {} [text "Hello"]
        world = text " World"
```

Technically, all React components accept children under the hood and a
component may choose to ignore them. We distinguish between `trees` and
`leaves` in Reactix only to make the types simpler in the `leaf` case.

## Abstracting DOM Elements

Let us say we wish to render lots of HRs on a page with the same
`height`.  Let us create a function `thinHR` which always has a
`height` of `1` but accepts a `width`:

```purescript
import Prelude
import Reactix (Element)
import Reactix.DOM.Raw (hr)

thinHR :: String -> Element
thinHR w = hr {height: 1, width: w}
```

In fact, `hr` is itself a function:

```purescript
hr :: forall props. Record props -> Element
hr props = createDOMElement "hr" props []
```

We will look in more detail at `createDomElement` next.

## Creating DOM Elements

Let us look again at how `hr` is implemented:

```purescript
-- createDOMElement :: forall props. String -> Record props -> Array Element -> Element

hr :: forall props. Record props -> Element
hr props = createDOMElement "hr" props []
```

`createDOMElement` is like `document.createElement`, but returns a
React `Element`. It is everything we need to build all the HTML
we desire. The `props` it accepts map to DOM properties.

Important: *DOM properties do not always line up with HTML attribute names*. Ex:

* attribute `class` -> property `className`

You can get a full list from [the mozilla developer website](https://developer.mozilla.org/en-US/docs/Web/API/Element).

## Introduction to components

We have not yet actually defined a component, so let us do that:
n
> A component is a specification for creating an `Element` from
> some `props` and `children`.

In Reactix, this means either DOM Elements (with `createDOMElement` as
above) or function components, which we haven't met yet.

Let's rectify that and meet function components!

### What is a function component?

> A function component is a function from `props` to `Element`

Like DOM elements can be either `leaf` or `tree` components, so too
can function components:

| Children? | Term   | 
|-----------|--------|
| Yes       | `tree` |
| No        | `leaf` |

We break components down by another property: `purity`. This
corresponds roughly to the regular purescript notion of purity:

> A function is pure if its inputs fully determine its output.

Or more relatably in this context:

> A function component is pure if its props and children fully
> determine its output.

So there are four possibilities of function components

| Name         | Uses Hooks | Accepts Children |
|--------------|------------|------------------|
| `pure leaf`  | No         | No               |
| `pure tree`  | No         | Yes              |
| `hooks leaf` | Yes        | No               |
| `hooks tree` | Yes        | Yes              |

Pure components are simpler, so let's look at those first!

## Pure components

### Pure leaves

Pure leaf components are the easiest type to write - they are a simple
function from `props` to `Element`:

```purescript
import Prelude
import Reactix (Element, createLeaf, pureLeaf)
import Reactix.DOM.Raw (span, b, text)

-- Specify our Props type upfront
type Props = ( greeting :: String, name :: String )

example :: Element
example = createLeaf greeter {greeting: "Hello", name: name}

greeter :: Leaf Props -- our actual component
greeter = pureLeaf cpt
  where cpt props = span {} [ greeting props, name props]
        greeting props = b {} [text props.greeting]
        name props = text (", " <> props.name)
```

As we hinted at earlier, all components can technically have children
in pure react, so `createLeaf` is really just a convenience for
writing childless components. You will also want to create components
which can have children: our next topic!

### Pure trees

Like pure leaf components, pure tree components are functions of
type `props -> Element`. But since we have learned that pure
components are fully determined by their `props` and `children`, we
have to ask: "where are the children?".

Well, they're in the `props`! Your function gets passed a copy of
props with an extra key: `children` (of type `Children`)!

Now, in a strongly typed language, these are two different types. We
have the handy `WithChildren` type alias to help!

```purescript
import Reactix (Children)

type WithChildren props = ( children :: Children | props )
```

Here is a simple example that wraps its children in a div with a color
taken from its props:

```purescript
import Prelude
import Reactix (Element, WithChildren, createTree, children)
import Reactix.DOM.Raw (div)

type Props = ( color :: String )

example :: Element
example = wrap { color: "red" } [ div {} [ text "Hello World" ] ]

wrap :: Record Props -> Array Element -> Element
wrap p c = createTree wrapper p c
  where wrapper :: Record (WithChildren Props) -> Element
        wrapper props = div {color: props.color} (children props.children)
```

The `children` function takes a React `Children` type and turns it
into an `Array Element` so we can pass it directly to our `div`
function. In fact, we have a convenience function `pureTree` which
turns a function of type `Record props -> Array Element -> Element`
into a `Record (WithChildren props) -> Element`. Example:

```purescript
import Prelude
import Reactix (Element, WithChildren)
import Reactix.DOM.Raw (div)

type Props = ( color :: String )

example :: Element
example = wrap { color: "red" } [ div {} [ text "Hello World" ] ]

wrap :: Record Props -> Array Element -> Element
wrap p c = createTree wrapper p c

wrapper :: Record (WithChildren Props) -> Element
wrapper = pureTree cpt
  where cpt :: Record Props -> Array Element -> Element
        cpt props children' = div {color: props.color} children
```

## Hooks components

Now the fun begins!

Hooks allow us to create components that do more than pure
components. They can maintain state and perform side effects and
generally build complex and powerful webapps.

Let's start with a simple leaf component first.

### Hooks leaves

```purescript
import Prelude
import Effect (Effect)
import Reactix (Hooks, Element, createLeaf, hooksLeaf, useState, read, write)
import Reactix.DOM.Raw (div, button)

-- Specify our Props type upfront
type Props = ( initialCount :: Int)

example :: Element
example = counter { initialCount: 0 }

counter :: Record Props -> Element
counter props = createLeaf counterCpt props

-- Our actual component
counterCpt :: Record Props -> Element
counterCpt = hooksLeaf cpt
  where cpt props =
    do count <- useState props.initialCount
       let clickHandler = \_ -> write count (read count + 1)
       pure $ div {}
         [ text (show $ read count)
         , button {onClick: clickHandler} [ text "Click me"] ]
```

Yes, that counter really will increment when the button is pressed.

Hooks are more powerful than this little taster, but before we cover
them in more detail, let's meet our last type of component.

### Hooks trees








### Tangent: Components and the component tree

If we hadn't written a component, we wouldn't have to call
`createElement` at all, so why have we chosen to do so here?

Unfortunately, that's not a straightforward question, so we need to
explain what happens under the hood...

You might ask what we've gained over a function that is not a
component here, and it's a valid question, but it needs a longer
answer...

From a React development perspective, most of the useful work is done
by the `createElement` function. Logically, we construct a tree of
`Element` with it, which we call the `virtual dom`. This is a template
that React can use to render it to the real DOM.

The `render` function takes this virtual DOM and binds it to the real
DOM at a location of your choice. This is a binding in perpetuity,
i.e. if the virtual DOM changes at any point in time, the live DOM
will be updated accordingly to match. 


We use the `render` function to bind it to the real DOM. `render`
lives in `Effect` because it is blatantly effectful. When we execute
`render`, we are asking react to copy our virtual DOM to the real
DOM. In fact, it keeps on synchronising it when anything changes.

Our tree of `Element` is what we call the 'virtual DOM'. Before rerendering the DOM

, then is a data structure that react uses to

So our tree of Element is really called the 'virtual DOM'. When it
changes, React will rerender it to the live DOM automatically once
we've made that first `render` call. But how does it change?


What is interesting is that if the
properties you create an element with do not change, React will not
rerender all of those components to the DOM
