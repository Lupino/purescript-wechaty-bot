var fetchJSON = require('higher-order-helper').fetchJSON;

exports.startsWith = function(s) {
  return function(s1) {
    return s.startsWith(s1);
  }
}

exports._fetchJSON = function(url) {
  return function(options) {
    return function() {
      return fetchJSON(url, options);
    }
  }
}
