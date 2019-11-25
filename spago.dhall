{ name =
    "nodetrout"
, license =
    "MIT"
, repository =
    "https://github.com/nsaunders/purescript-nodetrout.git"
, dependencies =
    [ "argonaut"
    , "b64"
    , "console"
    , "effect"
    , "encoding"
    , "form-urlencoded"
    , "http-methods"
    , "node-http"
    , "psci-support"
    , "spec"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
