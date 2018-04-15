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

var re_emoji = /<img[^\[]+\[([^\]]+)][^>]+>/gi;
exports._getContent = function(msg) {
  return function() {
    var content = msg.content();
    content = content.replace(/<br[^>]*>/ig, "\n");
    content = content.replace(/<\/?a[^>]*>/ig, "");
    content = content.replace(re_emoji, function(s) {
      m = re_emoji.exec(s);
      return '[' + m[1] + ']';
    });
    return content;
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
