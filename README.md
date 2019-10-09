# purescript-nodetrout
## Build a Node server using Trout

[Trout](https://github.com/purescript-hyper/purescript-trout) is a type-level routing DSL very similar to Haskell's [Servant](https://github.com/haskell-servant/servant). This library provides a Trout server implementation that is directly compatible with [`purescript-node-http`](https://pursuit.purescript.org/packages/purescript-node-http/5.0.2/docs/Node.HTTP#v:createServer) given its type of `Request -> Response -> Effect Unit`.

More documentation is coming soon. For now, please review the [examples](example) and [tests](test].
