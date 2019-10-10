let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/master/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/master/src/packages.dhall sha256:d259540d220aae7e707b1c55332e4ea602dadab50289b047956967eb5d533108

let overrides = {=}

let additions = {=}

let additions = {=}

in  upstream // overrides // additions
