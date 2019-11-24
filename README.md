# purescript-nodetrout [![build status](https://img.shields.io/travis/nsaunders/purescript-nodetrout.svg)](https://travis-ci.org/nsaunders/purescript-nodetrout) [![purescript-nodetrout on Pursuit](https://pursuit.purescript.org/packages/purescript-nodetrout/badge)](https://pursuit.purescript.org/packages/purescript-nodetrout) [![nodetrout in package-sets](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-zxa7vacp3dju.runkit.sh/nodetrout)](https://github.com/purescript/package-sets)
## Build a Node server with Trout.

<img src="https://raw.githubusercontent.com/nsaunders/purescript-nodetrout/master/img/readme-tile.png" alt="purescript-nodetrout" align="right" />

[Trout](https://github.com/purescript-hyper/purescript-trout) is a type-level routing DSL similar to Haskell's [Servant](https://github.com/haskell-servant/servant) library. Given route specifications defined with Trout and their corresponding handlers, this library produces a [`node-http`](https://github.com/purescript-node/purescript-node-http) request handler of type `Request -> Response -> Effect Unit`, which [can be used to create an HTTP server](https://pursuit.purescript.org/packages/purescript-node-http/5.0.2/docs/Node.HTTP#v:createServer).

Assuming some familiarity with Servant or Trout, the best way to get started with this library is to review the [examples](example) and [tests](test).

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
