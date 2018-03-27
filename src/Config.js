var config = require("./config");

exports.get = function(k) {
  return config[k];
}
