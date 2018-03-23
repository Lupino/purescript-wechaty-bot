var wechaty = require('wechaty');
var Contact = wechaty.Contact;

exports._find = function(name) {
  return Contact.find({name: name});
}

exports._say = function(contact, obj) {
  return contact.say(obj);
}

exports.getContactId = function(contact) {
  return contact.id;
}

exports.getContactName = function(contact) {
  return contact.name();
}
