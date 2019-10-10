{ name =
    "nodetrout"
, dependencies =
    [ "argonaut"
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
