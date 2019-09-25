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
    , "strings-extra"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
