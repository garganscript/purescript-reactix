let upstream =
      https://github.com/garganscript/package-sets/releases/download/v0.1.4/release.dhall sha256:e03eafe0c7ea0ac143d07ec6d9f20c804bd6b6f95a8d89bf287c279e770584c8

let additions =
      { inflection =
        { dependencies = [ "functions" ]
        , repo = "https://github.com/athanclark/purescript-inflection"
        , version = "v1.0.1"
        }
      }

in  upstream ⫽ additions
