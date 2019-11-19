{ name =
    "nodetrout"
, dependencies =
    [ "argonaut"
    , "b64"
    , "console"
    , "effect"
    , "form-urlencoded"
    , "http-methods"
    , "node-http"
    , "profunctor-lenses"
    , "psci-support"
    , "spec"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
