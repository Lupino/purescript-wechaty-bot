const fetchJSON = require('higher-order-helper').fetchJSON;
const dayjs = require('dayjs');

exports.startsWith = function(s) {
  return function(s1) {
    return s.startsWith(s1);
  };
};

exports._fetchJSON = function(url) {
  return function(options) {
    return function() {
      return fetchJSON(url, options);
    };
  };
};

exports.formatDate = function(date) {
  return dayjs(date * 1000).format('YYYY-MM-DD HH:mm:ss');
}
