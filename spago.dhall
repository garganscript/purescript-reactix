{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "reactix"
, dependencies =
  [ "aff"
  , "arrays"
  , "dom-simple"
  , "effect"
  , "ffi-simple"
  , "foldable-traversable"
  , "functions"
  , "maybe"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "refs"
  , "spec"
  , "spec-mocha"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
