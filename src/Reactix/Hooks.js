'use strict';
var React = require("react");

function _simple(prop) {
  return function() { return React[prop].apply(React, arguments); };
}
function _memo(prop) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(args.pop());
    return React[prop].apply(React, args);
  }
}
exports._tuple = function tuple(ctor, v) { return ctor(v[0])(v[1]); };
exports._tupleCurrent = function tupleCurrent(ctor, ref) {
  const set = function(v) { ref.current = v; };
  return ctor(ref.current)(set);
};
exports._useContext = _simple('useContext');
exports._useDebugValue = _simple('useDebugValue');
exports._useDebugValuePrime = _simple('useDebugValue');
// exports._useImperativeHandle = _simple('useImperativeHandle');
exports._useMemo = _simple('useMemo');
exports._useMemo1 = _memo('useMemo');
exports._useMemo2 = _memo('useMemo');
exports._useMemo3 = _memo('useMemo');
exports._useMemo4 = _memo('useMemo');
exports._useMemo5 = _memo('useMemo');

