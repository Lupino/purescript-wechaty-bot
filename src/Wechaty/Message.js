exports._say = function(msg, obj) {
  return function() {
    return msg.say(obj);
  }
}

exports._sayTo = function(msg, replyTo, obj) {
  return function() {
    return msg.say(obj, replyTo);
  }
}

exports._getContent = function(msg) {
  return function() {
    return msg.content();
  }
}

exports._getFrom = function(msg) {
  return function() {
    return msg.from();
  };
}

exports._getSelf = function(msg) {
  return function() {
    return msg.self();
  }
}

exports._room = function(just, nothing, msg) {
  return function() {
    if (msg.room()) {
      return just(msg.room());
    } else {
      return nothing;
    }
  }
}
