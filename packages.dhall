let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220510/packages.dhall sha256:0b0d4db1f2f0acd3b37fa53220644ac6f64cf9b5d0226fd097c0593df563d5be


let additions =
      { dom-simple =
          { dependencies =
             [ "aff"
             , "arrays"
             , "effect"
             , "ffi-simple"
             , "maybe"
             , "nullable"
             , "prelude"
             , "unsafe-coerce"
             , "spec"
            ]
          , repo = "https://github.com/garganscript/purescript-dom-simple"
          , version = "ps-15.0-upgrade"
        }
      , ffi-simple =
        { dependencies = 
          [ "aff"
          , "arrays"
          , "effect"
          , "foldable-traversable"
          , "functions"
          , "maybe"
          , "nullable"
          , "prelude"
          , "refs"
          , "spec"
          , "tuples"
          , "unsafe-coerce"
          , "unsafe-reference" ]
        , repo = "https://github.com/garganscript/purescript-ffi-simple"
        , version = "v0.3.1"
        }
      , inflection =
        { dependencies =
            [ "functions" ]
        , repo = "https://github.com/athanclark/purescript-inflection"
        , version = "v1.0.1"
	}
      , spec-mocha =
        { dependencies =
            [ "console", "foldable-traversable", "exceptions", "spec" ]
        , repo = "https://github.com/purescript-spec/purescript-spec-mocha"
        , version = "v4.0.0"
	}
      }

in  upstream // additions
