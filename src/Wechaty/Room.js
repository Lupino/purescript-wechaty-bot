var wechaty = require('wechaty');
var Room = wechaty.Room;

exports._find = function(topic) {
  return function(just) {
    return function(nothing) {
      return function() {
        return Room.find({topic: topic})
          .then(function(r) {
          if (r) {
            return just(r);
          } else {
            return nothing;
          }
        });
      }
    }
  }
}

exports._findAll = function(topic) {
  return function() {
    return Room.findAll({topic: new RegExp(topic)});
  }
}

exports._say = function(room, obj) {
  return function() {
    return room.say(obj);
  }
}

exports._sayTo = function(room, replyTo, obj) {
  return function() {
    return room.say(obj, replyTo);
  }
}

exports.getRoomTopic = function(room) {
  return room.topic();
}
