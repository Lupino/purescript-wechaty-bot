var wechaty = require('wechaty');
var Wechaty = wechaty.Wechaty;
var config = wechaty.config;
var QrcodeTerminal = require('qrcode-terminal');

exports.initWechaty = function() {
  var bot = Wechaty.instance({profile: config.default.DEFAULT_PROFILE});
  bot.on('scan', function (url, code) {
    if (!/201|200/.test(String(code))) {
      var loginUrl = url.replace(/\/qrcode\//, '/l/')
      QrcodeTerminal.generate(loginUrl);
    }
    console.log(url + '\n[' + code + '] Scan QR Code above url to log in: ');
  });
  bot.on('error', function(e) {
    console.error('Bot', 'error: %s', e)
    if (bot.logonoff()) {
      bot.say('Wechaty error: ' + e.message).catch(console.error)
    }
  });
  return bot;
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
      return callback(message)();
    });
  }
}

exports._start = function(bot) {
  return function() {
    return bot.start();
  }
}
