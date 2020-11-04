"use strict";
const streamBuffers = require('stream-buffers');

exports.mkStreamBuffer = function (pure) {
    return pure(new streamBuffers.WritableStreamBuffer());
}

exports.getStreamBufferContents = function(pure) {
  return function(sb) {
    return pure(sb.getContents());
  }
}