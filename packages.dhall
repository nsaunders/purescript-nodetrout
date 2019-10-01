let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20190818/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20190818/src/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides = {=}

let additions = {=}

let additions =
      { trout =
          mkPackage
          [ "argonaut"
          , "media-types"
          , "prelude"
          , "smolder"
          , "uri"
          ]
          "https://github.com/purescript-hyper/purescript-trout.git"
          "929be9bfb9122fcd0ace6f63f6412a6b13d328b8"
      , http =
          mkPackage
          [ "maybe" ]
          "https://github.com/joneshf/purescript-http.git"
          "v4.0.0"
      }

in  upstream // overrides // additions
