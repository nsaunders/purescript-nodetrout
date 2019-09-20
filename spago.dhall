{ name =
    "nodetrout"
, dependencies =
    [ "argonaut"
    , "argonaut-generic"
    , "console"
    , "effect"
    , "form-urlencoded"
    , "http"
    , "http-methods"
    , "node-http"
    , "psci-support"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
