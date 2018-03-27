var wechaty = require('wechaty');
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

exports.getContactId = function(contact) {
  return contact.id;
}

exports.getContactName = function(contact) {
  return contact.name();
}
