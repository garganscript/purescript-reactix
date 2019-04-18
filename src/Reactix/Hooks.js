'use strict';
var React = require("react");

function _simple(prop) {
  return function() { return React[prop].apply(React, arguments); };
}
function _tuple(prop) {
  return function(ctor) {
    const r = React[prop].apply(React, Array.prototype.slice.call(arguments, 1));
    return ctor(r[0])(r[1]);
  }
}
function _memo(prop) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(args.pop());
    return React[prop].apply(React, args);
  }
}

exports._useContext = _simple('useContext');
exports._useDebugValue = _simple('useDebugValue');
exports._useDebugValuePrime = _simple('useDebugValue');
// exports._useImperativeHandle = _simple('useImperativeHandle');

exports._useState = _tuple('useState');

exports._useReducer = _tuple('useReducer');

exports._useRef = function(ctor, value) {
  const r = React.useRef(value);
  const set = function(v) { r.current = v; };
  return ctor(r.current)(set);
};

exports._useMemo = _simple('useMemo');
exports._useMemo1 = _memo('useMemo');
exports._useMemo2 = _memo('useMemo');
exports._useMemo3 = _memo('useMemo');
exports._useMemo4 = _memo('useMemo');
exports._useMemo5 = _memo('useMemo');

exports._useEffect = _simple('useEffect');
exports._useEffect1 = _memo('useEffect');
exports._useEffect2 = _memo('useEffect');
exports._useEffect3 = _memo('useEffect');
exports._useEffect4 = _memo('useEffect');
exports._useEffect5 = _memo('useEffect');

exports._useLayoutEffect = _simple('useLayoutEffect');
exports._useLayoutEffect1 = _memo('useLayoutEffect');
exports._useLayoutEffect2 = _memo('useLayoutEffect');
exports._useLayoutEffect3 = _memo('useLayoutEffect');
exports._useLayoutEffect4 = _memo('useLayoutEffect');
exports._useLayoutEffect5 = _memo('useLayoutEffect');
