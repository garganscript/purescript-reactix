'use strict';
var React = require("react");

function _simple(prop) {
  return function() { return React[prop].apply(React, arguments); };
}

exports._useContext = _simple('useContext');
exports._useDebugValue = _simple('useDebugValue');
exports._useDebugValuePrime = _simple('useDebugValue');
// exports._useImperativeHandle = _simple('useImperativeHandle');

exports._useState = function(init) {
  const r = React.useState(init);
  const set = function(s) { r[1](s); };
  return { value: r[0], set: set };
};

exports._call1 = function(state, value) { state[1](value); };
exports._read0 = function(state) { return state[0]; };

exports._useReducer = _simple('useReducer');

exports._useRef = function(value) {
  const r = React.useRef(value);
  return { value: r.current, set: function(v) { r.current = v; } };
};
exports._readCurrent = function(v) { return v.current; };
exports._writeCurrent = function(v,w) { v.current = w; };

function _memo(prop) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(args.pop());
    return React[prop].apply(React, args);
  }
}
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
