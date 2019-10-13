let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20191005/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20191005/src/packages.dhall sha256:ba287d858ada09c4164792ad4e643013b742c208cbedf5de2e35ee27b64b6817

let overrides = {=}

let additions = {=}

let additions = {=}

in  upstream // overrides // additions
