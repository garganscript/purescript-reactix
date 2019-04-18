'use strict';
const utils = require('react-dom/test-utils');
const test = require('react-testing-library');

function spread(obj,prop) {
  return function() {
    return obj[prop].apply(obj, Array.from(arguments));
  };
}
exports._act = function(e) {
  var ret;
  utils.act(function() { ret = e(); });
  return ret;
};

exports._render = spread(test,'render');
exports._fireEvent = function(name, obj) {
  test.fireEvent[name].call(test.fireEvent, obj);
};
