const dayjs = require('dayjs');

exports.fromUnixTime = function(t) {
  return dayjs(t * 1000);
};

exports.toUnixTime = function(t) {
  return t.unix();
};

exports.set = function(string) {
  return function(int) {
    return function(t) {
      return t.set(string, int);
    };
  };
};

exports.add = function(num) {
  return function(units) {
    return function(t) {
      return t.add(num, units);
    };
  };
};

exports.format = function(formatStr) {
  return function(t) {
    return t.format(formatStr);
  };
};

exports.now = function() {
  return dayjs();
};
