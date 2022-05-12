# purescript-reactix

A minimal, modern react binding focusing on ease-of-use

## Status: alpha

Features are being added and tested as I need them for work.

Hooks believed to work correctly (including variants)

* `useState`
* `useReducer`
* `useContext`
* `useRef`
* `useEffect`
* `useLayoutEffect`

## Rationale

For a long time, the only way to do react was with class
components. Libraries to make this easier popped up and we all ended
up writing code with them. It is often verbose and difficult to reason
about, but it works.

React 1.4 introduced stateless function components, a simpler way to
build simple components. They are basically stateless functions taking
a set of props and returning a React Element. The lucky users whose
libraries supported stateless function components gained a simple
model for building and reasoning about simple React components.

React 16.8 introduced 'hooks', turbocharging function components and
making them equivalent in power to class components. Hooks are side
effecting functions that can be used within a function component in
order to tap into the extra facilities provided by the React library.

Hooks are IMHO a breakthrough feature. They're simple to reason about
and they're low on boilerplate. They make React programming much more
fun and productive and I never want to touch a class component again.

Reactix is a modern, Hooks-first React library focusing on simplicity
and ease-of-use.

<!-- ## Tutorial -->

<!-- There is a tutorial in `docs/tutorial.md` or [online](https://github.com/irresponsible/purescript-reactix/blob/master/docs/tutorial.md) -->

<!-- ## Usage -->

## Interop

If you wish to use this library with other react libraries, you will
need to write a little glue. The minimum you can sanely get away with
is a typed wrapper over `unsafeCoerce` between `Element` and whatever your
other library's element type is.

## Alternatives

If you already use `purescript-react-basic`, you may prefer
[purescript-react-basic-hooks](https://github.com/spicydonuts/purescript-react-basic-hooks),
a similar library implemented on top of `purescript-react-basic`.


## TODO

Not in any particular order

1. Safe DOM props:
  * Make sure each element only takes the correct props
2. Refs
  * Test forwardRef
3. Context
  * Test consumer (we only test useContext right now)
4. Misc hooks
  * Test useEffect/useLayoutEffect are fired at the correct stage
  * Test useEffectOnce/useLayoutEffectOnce are fired once
  * Test useMemo/useCallback
  * Test useImperativeHandle
  * Test useDebugValue
  * Test React.isValid
5. Setter - a shorthand for setter functions
6. Reductor - a reducer that lives in Effect
  1. Actor - a shorthand for the transformer
7. Stator - a state that lives in Effect

Ideas we're not yet convinced on:

* Move to `Aff` instead of `Effect` (not currently convinced)

## Changelog

<!-- ### NEXT -->

<!-- Newly supported functions: -->

<!-- * `R.React.consumeContext` -->
<!-- * `R.React.provide` - provider a value through a `Provider` -->
<!-- * `R.React.consume` - consume a value through a `Consumer` -->

## 0.5.0

Changed:

* Migrate to purescript 0.15.0
* add `test.dhall` to keep purely test dependencies. See
  https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
  for more info

### 0.4.2

New:

* `useEffectOnce` (+ layout and prime variants) - effects which run
  once at component mount and cleanup once at component dismount.
* `unsafeUseEffect`, `unsafeUseLayoutEffect`, `unsafeUseMemo`,
  `unsafeUseCallback`, `unsafeUseImperativeHandle` - unsafe hook
  variants where you pass an array-like object of memo values without
  any help from the type system to assert such likeness to an array.

### 0.4.1

New:

* `useEffectFn{1-5}` (+ layout and prime variants) - variants taking
  functions from memo values to effect, to better aid code reuse

### 0.4.0

Breaking:

* `useState` now takes a (pure) `Unit -> s` initialiser
* `useState`'s setter function now takes a pure fn `state -> state`.
  To get back the old behaviour of setting a value without
  regard to its existing value, use `const`.
* `useReducer` now takes a (pure) `i -> s` initialiser
* `useEffect`, `useLayoutEffect` and their numbered variants no longer
  take a redundant dummy unit value to delay computation.

New:

* 'Magic' DOM Props. Each takes a record of props which will be transformed
  * `aria` - prop names will be prefixed with `aria-`
  * `data` - prop names will be prefixed with `data-`
  * `on` - prop names will be prefixed with `on`, values will be `mkEffectFn1`'d

Example:

```purescript
import Reactix as R
import Reactix.DOM.HTML (div, text)
import DOM.Simple.Console (log)

ex :: R.Element
ex =
  div
    { aria: {label: "example"}
    , data: {thing: Just 1}
    , on: {click: \_ -> log "Hello World"}
    }
    [ text "Hello World" ]
```

### 0.3.1

Bug Fixes:

* `R.DOM.HTML` - more or less everything was broken since we moved out `createDOMElement`

### 0.3.0

Newly supported hooks and variants:

* `R.Hooks.useReducer`
* `R.Hooks.useReducer'` (`useReducer` with a constant initial value)
* `R.Hooks.useContext`
* `R.Hooks.useState'` (`useState` with a constant initial value)
* `R.Hooks.useEffect[1-5]'` (`useEffect` with no cleanup function)
* `R.Hooks.useLayoutEffect[1-5]'` (`useLayoutEffect` with no cleanup function)

Newly supported functions:

* `R.React.createContext` - create a `Context` with a default value
* `R.React.provideContext` - provide a `Context` a value to some children
* `R.React.provider` - get a `Provider` for a `Context`
* `R.React.consumer` - get a `Consumer` for a `Context`
* `R.Hooks.nothing` - an empty cleanup function
* `R.Hooks.thenNothing` - make an effect function return an empty cleanup function

Changes:

* `R.createDOMElement` is now how you create DOM elements, we removed
  the `R.IsComponent` instance for `String`

Bug Fixes:

* `R.React.render` was broken because the tests don't use it and
  neither does the main consumer of this project where it gets put
  through its paces

Misc:

* First attempts at all remaining hook functions have been written and
  not tested. Don't be surprised if they don't work.
* Remove foreign javascript file for `R.Hooks` in favour of `ffi-simple`

### 0.2.0

* Make SyntheticEvent parameterised by a native event

### 0.1.1

* Add a, li, nav, ul tags to R.DOM.Raw
* Add `R.Hooks.useRef`

### 0.1.0

* First numbered release
* Made `useState`, `useEffect` and `useLayoutEffect` take dummy
  `Unit`s to delay execution until the appropriate time

Supported Hooks:

* `useState`
* `useEffect[1-5]?`
* `useLayoutEffect[1-5]?`

Notable changes:

* Major refactor to use [ffi-simple](https://github.com/irresponsible/purescript-ffi-simple).

## Thanks

- [Nicolas Pouillard](https://github.com/np), for the discussions that
  continue to shape the design of reactix.
- The rest of the [gargantext](https://gitlab.iscpif.fr/gargantext/purescript-gargantext)
  team, for their enthusiasm both for reactix and for replacing thermite.

## Copyright and License

Copyright (c) 2018 James Laver

This software is free and open source software licensed under the
terms of the Mozilla Public License (MPL) 2.0

