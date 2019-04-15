# purescript-reactix-lite

A minimal purescript binding to React Hooks

## Status: alpha

All hooks are implemented, few are tested well, if at all.

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

## Concepts

### Pure components



### Hooks components

## Limitations

## Alternatives

This library uses React directly and knows nothing of any other
purescript React libraries. If you already use a react library, you
should consider one of these libraries instead:

- [purescript-reactix-react](https://github.com/irresponsible/purescript-reactix-react) (a fork of this library to support purescript-react)
- [purescript-react-basic-hooks](https://github.com/spicydonuts/purescript-react-basic-hooks) (a similar library implemented on top of purescript-react-basic)

# Gluing 

If you wish to use this library directly anyway, you will need to
write a little glue.

# Usage



# TODO

More, better tests.

## Copyright and License

Copyright (c) 2018 James Laver

This software is free and open source software licensed under the
terms of the Mozilla Public License (MPL) 2.0

