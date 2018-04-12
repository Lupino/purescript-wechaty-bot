var wechaty = require('wechaty');
var Wechaty = wechaty.Wechaty;
var config = wechaty.config;
var QrcodeTerminal = require('qrcode-terminal');

exports.initWechaty = function() {
  return Wechaty.instance({profile: config.default.DEFAULT_PROFILE});
}

exports._onScan = function(bot) {
  return function(callback) {
    return function() {
      bot.on('scan', function(url, code) {
        callback(url)(code)();
      });
    }
  }
}

exports.showQrcode = function(url) {
  return function() {
    var loginUrl = url.replace(/\/qrcode\//, '/l/')
    QrcodeTerminal.generate(loginUrl);
  }
}

exports._onLogout = function(bot, callback) {
  return function() {
    bot.on('logout', function(user) {
      callback(user)();
    });
  }
}

exports._onLogin = function(bot, callback) {
  return function() {
    bot.on('login', function(user) {
      callback(user)();
    });
  }
}

exports._onMessage = function(bot, callback) {
  return function() {
    bot.on('message', function(message) {
      if (message.type() === 1) {
        return callback(message)();
      }
    });
  }
}

exports._start = function(bot) {
  return function() {
    return bot.start();
  }
}

exports._onError = function(bot) {
  return function(callback) {
    return function() {
      bot.on('error', function(e) {
        callback(e.message)();
        if (bot.logonoff()) {
          bot.say('Wechaty error: ' + e.message).catch(console.error)
        }
      });
    }
  }
}
