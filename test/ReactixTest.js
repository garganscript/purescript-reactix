"use strict";
const React = require('react');

exports._raf = function(f) { window.requestAnimationFrame(function() { f(); }); };
exports._st = function(f,t) { window.setTimeout(f,t); };
exports._fakeStatic = function(r, p) {
  var body = "" + p.count;
  React.useEffect( function() { r.value += 1; } );
  return React.createElement(
    React.Fragment,
    {},
    React.createElement(
      "button", {}, "++"
    ),
    React.createElement(
      "i", {}, "" + p.count
    )
  );
};
