let someStatefulVariable = 0;
exports.incrementStatefulVariable = function() {
  let oldValue = someStatefulVariable;
  someStatefulVariable += 1;
  return oldValue;
}

exports.getStatefulVariable = function () {
  return someStatefulVariable;
}