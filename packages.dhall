let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210629/packages.dhall sha256:534c490bb73cae75adb5a39871142fd8db5c2d74c90509797a80b8bb0d5c3f7b

let additions =
      { dom-simple =
        { dependencies =
            [ "console"
            , "effect"
            , "functions"
            , "nullable"
            , "prelude"
            , "spec"
            , "spec-mocha"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/poorscript/purescript-dom-simple"
        , version = "v0.2.7"
	}
      , ffi-simple =
        { dependencies =
            [ "prelude"
            , "effect"
            , "maybe"
            , "functions"
            , "nullable"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/poorscript/purescript-ffi-simple"
        , version = "v0.2.10"
	}
      , inflection =
        { dependencies =
            [ "functions" ]
        , repo = "https://github.com/athanclark/purescript-inflection"
        , version = "v1.0.0"
	}
      , spec-mocha =
        { dependencies =
            [ "console", "foldable-traversable", "exceptions", "spec" ]
        , repo = "https://github.com/purescript-spec/purescript-spec-mocha"
        , version = "v4.0.0"
	}
      }

in  upstream // additions
