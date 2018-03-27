var wechaty = require('wechaty');
var Room = wechaty.Room;

exports._find = function(topic) {
  return function() {
    return Room.find({topic: topic});
  }
}

exports._say = function(room, obj) {
  return function() {
    return room.say(obj);
  }
}

exports._sayTo = function(room, replyTo, obj) {
  return function() {
    return msg.say(room, replyTo);
  }
}

exports.getRoomId = function(room) {
  return room.id;
}

exports.getRoomTopic = function(room) {
  return room.topic();
}
