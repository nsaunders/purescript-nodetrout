# purescript-nodetrout [![build status](https://img.shields.io/travis/nsaunders/purescript-nodetrout.svg)](https://travis-ci.org/nsaunders/purescript-nodetrout) [![purescript-nodetrout on Pursuit](https://pursuit.purescript.org/packages/purescript-nodetrout/badge)](https://pursuit.purescript.org/packages/purescript-nodetrout) [![nodetrout in package-sets](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-zxa7vacp3dju.runkit.sh/nodetrout)](https://github.com/purescript/package-sets)
## Build a Node HTTP server with Trout.

<img src="https://raw.githubusercontent.com/nsaunders/purescript-nodetrout/master/img/readme-tile.png" alt="purescript-nodetrout" align="right" />

[Trout](https://github.com/purescript-hyper/purescript-trout) is a type-level routing DSL similar to Haskell's [Servant](https://github.com/haskell-servant/servant) library. Given route specifications defined with Trout and their corresponding handlers, this library produces a [`node-http`](https://github.com/purescript-node/purescript-node-http) request handler function of type `Request -> Response -> Effect Unit`. The request handler [can then be used to create an HTTP server](https://pursuit.purescript.org/packages/purescript-node-http/5.0.2/docs/Node.HTTP#v:createServer).

### An API in 4 simple steps
1. Specify routes as a data type. Here, we create a `GET /admin` route that requires a Basic Authorization header and responds with a greeting:
```
type Site = "admin" := "admin" :/ Header "Authorization" BasicAuth :> Resource (Get Greeting JSON)
```

2. Create a [`Proxy`](https://pursuit.purescript.org/packages/purescript-proxy/3.0.0/docs/Type.Proxy) value to capture the route specification:
```
site :: Proxy Site
site = Proxy
```

3. Define a handler for each route. Here, we greet the user by the username specified in the Basic Authorization header:
```
resources :: forall m. Monad m => { admin :: BasicAuth -> { "GET" :: ExceptT HTTPError m Greeting } }
resources = { admin: \auth -> { "GET": pure $ Greeting $ "Hello, " <> (fst $ un BasicAuth auth) } }
```

4. Serve the API using `node-http`:
```
main :: Effect Unit
main = do
  server <- createServer $ serve' site resources (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
```

### Examples

A number of usage examples are available [here](example).

### Installation

[bower](https://github.com/bower/bower):
```
bower install --save purescript-nodetrout
```

[psc-package](https://github.com/purescript/psc-package):
```
psc-package install nodetrout
```

[spago](https://github.com/spacchetti/spago):
```
spago install nodetrout
```

### Examples

To run the examples, clone the repository and run one of the following depending on your package manager and build tool, replacing `<example-name>` with the name of one of the [examples](example).

[bower](https://github.com/bower/bower) + [pulp](http://github.com/purescript-contrib/pulp):
```
bower install
pulp run -I example -m Example.<example-name>
```

[spago](https://github.com/spacchetti/spago):
```
spago run -p example/<example-name>.purs -m Example.<example-name>
```
