'use strict';

var React = require("react");
var react_dom = require('react-dom');

function _simple(prop) {
  return function() { return React[prop].apply(React, arguments); };
}

exports._isValid = _simple('isValidElement');

exports._children = function(c) { return React.Children.toArray(c); };

exports._memo = _simple('memo');
exports._memoPrime = _simple('memo');

exports._createRef = function() { return React.createRef(); };
exports._deref = function(r) { return r.current; };
// https://reactjs.org/docs/react-api.html#reactforwardref
// exports._forwardRef = _simple('forwardRef');


// creating and cloning elements

exports._cloneElement = _simple('cloneElement');

function _createElement(elem, props, children) {
  var c = children.slice();
  c.unshift(props);
  c.unshift(elem);
  return React.createElement.apply(React, c);
}

exports._createElement = _createElement;
exports._createFragment = function(children) {
  return _createElement(React.Fragment, {}, children);
};

exports._contextProvider = function(c) { return c.Provider; };

exports._contextConsumer = function(c) { return c.Consumer; };

exports._createContext = function(ctor, val) {
  var c = React.createContext(val);
  return {provider: c.Provider, consumer: c.Consumer, context: c};
};

exports._render = function(a,b) { return react_dom.render(a,b); };

exports._named = function(name, f) { f.name = name; return f; };
