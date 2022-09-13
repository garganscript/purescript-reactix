let upstream =
      https://github.com/garganscript/package-sets/releases/download/v0.1.3/release.dhall
        sha256:c9baf942dc812fbcc9c8d505bac6778643eeacaa31c6f8df7411f6ae0788c684

let additions =
      { inflection =
        { dependencies = [ "functions" ]
        , repo = "https://github.com/athanclark/purescript-inflection"
        , version = "v1.0.1"
        }
      }

in  upstream // additions
