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

exports._say = function(contact, obj) {
  return function () {
    return contact.say(obj);
  }
}

exports.getContactName = function(contact) {
  return contact.name();
}
