'use strict';
var react_dom = require('react-dom');

exports._getElementBySelector = function(sel) {
  return window.top.document.getElementBySelector(sel);
};

exports._render = function(a,b) { return react_dom.render(a,b); };

