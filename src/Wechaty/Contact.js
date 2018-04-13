var wechaty = require('wechaty');
var Wechaty = wechaty.Wechaty;
var Contact = wechaty.Contact;

exports._find = function(name) {
  return function(just) {
    return function(nothing) {
      return function() {
        return Contact.find({name: name})
          .then(function(c) {
          if (c) {
            return just(c);
          } else {
            var bot = Wechaty.instance();
            var user = bot.self();
            if (user.name() === name) {
              return just(user);
            }
            return nothing;
          }
        });
      }
    }
  }
}

exports._findAll = function(name) {
  return function() {
    return Contact.findAll({name: new RegExp(name)})
      .then(function(c) {
        if (c.length === 0) {
          var bot = Wechaty.instance();
          var user = bot.self();
          if (user.name() === name) {
            return [user];
          }
        }
        return c;
      })
  }
}

exports._say = function(contact, obj) {
  return function () {
    return contact.say(obj);
  }
}

exports.getContactName = function(contact) {
  return contact.name();
}

exports.self = function() {
  var bot = Wechaty.instance();
  return bot.self();
}
