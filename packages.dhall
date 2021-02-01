let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/ea2423043df6c90f0de754977923b6c5dfdddcfc/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let additions =
      { dom-simple =
          mkPackage
            [ "console"
            , "effect"
            , "functions"
            , "nullable"
            , "prelude"
            , "spec"
            , "spec-mocha"
            , "unsafe-coerce"
            ]
            "https://github.com/poorscript/purescript-dom-simple"
            "v0.2.7"
      , ffi-simple =
          mkPackage
            [ "prelude"
            , "effect"
            , "maybe"
            , "functions"
            , "nullable"
            , "unsafe-coerce"
            ]
            "https://github.com/poorscript/purescript-ffi-simple"
            "v0.2.10"
      , inflection =
          mkPackage
            [ "functions" ]
            "https://github.com/athanclark/purescript-inflection"
            "v1.0.0"
      , spec-mocha =
          mkPackage
            [ "console", "foldable-traversable", "exceptions", "spec" ]
            "https://github.com/purescript-spec/purescript-spec-mocha"
            "v4.0.0"
      }

in  upstream // additions
