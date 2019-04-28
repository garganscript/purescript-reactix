# purescript-reactix

A minimal purescript binding to React Hooks

## Status: alpha

Features are being added and tested as I need them for work.

Hooks believed to work correctly:

* `useState`
* `useEffect`
* `useLayoutEffect`
* `useRef`

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

Reactix is a Hooks-first (Hooks-only?) React library focusing on
simplicity and ease-of-use.

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

* DOM
  * safe props
* Synthetic Events
  * Come up with a testing strategy
  * What do do about event targets?
  * Implement remaining
* React
  * Refs
  * isValid (test)
  * context (createContext, provider, consumer, provide, consume)
* Hooks
  * useEffect/useLayoutEffect
    * Test they're fired at the correct stage
  * useReducer
    * Tests
  * useMemo
    * Tests
  * useCallback
    * Tests
  * useRef
    * Tests
  * useContext
    * Tests
  * useImperativeHandle
    * Tests
  * useDebugValue
    * Tests

## Changelog

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
* `useEffect`
* `useLayoutEffect`

Notable changes:

* Major refactor to use [ffi-simple](https://github.com/irresponsible/purescript-ffi-simple).

## Copyright and License

Copyright (c) 2018 James Laver

This software is free and open source software licensed under the
terms of the Mozilla Public License (MPL) 2.0

