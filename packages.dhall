let upstream =
      https://github.com/garganscript/package-sets/releases/download/v0.1.2/release.dhall
        sha256:9ea93c613a9981c5a573ddee3fa4c63914040c6b4aeeeeaf4cbe43cf682cb38d

let additions =
      { inflection =
        { dependencies = [ "functions" ]
        , repo = "https://github.com/athanclark/purescript-inflection"
        , version = "v1.0.1"
        }
      }

in  upstream // additions
