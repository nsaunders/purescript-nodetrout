# purescript-nodetrout [![build status](https://img.shields.io/travis/nsaunders/purescript-nodetrout.svg)](https://travis-ci.org/nsaunders/purescript-nodetrout)
## Build a Node server using Trout

<img src="https://raw.githubusercontent.com/nsaunders/purescript-nodetrout/master/img/readme-tile.png" alt="purescript-nodetrout" align="right" />

[Trout](https://github.com/purescript-hyper/purescript-trout) is a type-level routing DSL similar to Haskell's [Servant](https://github.com/haskell-servant/servant) library. Given route specifications and handlers, this library produces a [`node-http`](https://github.com/purescript-node/purescript-node-http) request handler of type `Request -> Response -> Effect Unit`, which [can be used to create an HTTP server](https://pursuit.purescript.org/packages/purescript-node-http/5.0.2/docs/Node.HTTP#v:createServer).

The best way to get started is to review the [examples](example) and [tests](test).
