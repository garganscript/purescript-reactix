{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "reactix"
, dependencies =
  [ "dom-simple"
  , "effect"
  , "ffi-simple"
  , "foldable-traversable"
  , "functions"
  , "maybe"
  , "nullable"
  , "prelude"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
