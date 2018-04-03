var mongoose = require('mongoose')
  Schema = mongoose.Schema
  mongod = require('./config').mongod
  plugin = require('mongoose-auto-increment')
  autoIncrPlugin = plugin.plugin
  ;

mongoose.Promise = global.Promise;
mongoose.connect(mongod);

plugin.initialize(mongoose.connection);

function timestamp() {
  return Math.floor(new Date() / 1000);
}

var GroupSchema = new Schema({
  group:      {type: String, index: {unique: true}},
  user:       {type: String, index: true},
  name:       {type: String},
  repeat:     {type: Number, default: 0},
  created_at: {type: Number, default: timestamp},
});

var Group = mongoose.model('Group', GroupSchema);

var MessageSchema = new Schema({
  user:       {type: String, index: true},
  group:      {type: String},
  seq:        {type: String},
  content:    {type: String},
  sched_at:   {type: Number, default: timestamp},
  created_at: {type: Number, default: timestamp},
});

MessageSchema.index({ group: 1, seq: 1 }, { unique: true });

MessageSchema.plugin(autoIncrPlugin, { model: 'Message', field: 'id', startAt: 1 });

var Message = mongoose.model('Message', MessageSchema);

var SubscribeSchema = new Schema({
  user:       {type: String, index: true},
  group:      {type: String, index: true},
  created_at: {type: Number, default: timestamp},
});

SubscribeSchema.plugin(autoIncrPlugin, { model: 'Subscribe', field: 'id', startAt: 1 });

var Subscribe = mongoose.model('Subscribe', SubscribeSchema);

var RoomSubscribeSchema = new Schema({
  room:       {type: String, index: true},
  group:      {type: String, index: true},
  created_at: {type: Number, default: timestamp},
});

RoomSubscribeSchema.plugin(autoIncrPlugin, { model: 'RoomSubscribe', field: 'id', startAt: 1 });

var RoomSubscribe = mongoose.model('RoomSubscribe', RoomSubscribeSchema);

function toJSON(u) {
  if (u.toJSON) {
    return u.toJSON();
  }
  return u;
}

function unit() {
  return {}
}

exports._saveGroup = function(u) {
  return function() {
    return Group.findOne({group: u.group})
      .exec()
      .then(function(u0) {
        if (u0) {
          return u0;
        }
        return new Group({group: u.group, name: u.name, user: u.user}).save();
      })
      .then(unit);
  }
}

exports._getGroup = function(group) {
  return function(just) {
    return function(nothing) {
      return function() {
        return Group.findOne({group: group})
          .exec()
          .then(function (u) {
            if (u) {
              return just(toJSON(u));
            }
            return nothing;
          });
      }
    }
  }
}

exports._setGroupRepeat = function(group) {
  return function(repeat) {
    return function() {
      return Group.findOneAndUpdate({group: group}, {repeat: repeat})
        .exec()
        .then(unit)
    }
  }
}

exports._createMessage = function(msg) {
  return function() {
    return new Message({
      user: msg.user,
      group: msg.group,
      seq: msg.seq,
      content: msg.content,
      sched_at: msg.sched_at,
    }).save().then(unit);
  }
}

exports._updateMessage = function(msg) {
  return function() {
    return Message.findOneAndUpdate({
      group: msg.group,
      seq: msg.seq
    }, {
      content: msg.content,
      sched_at: msg.sched_at
    }).exec().then(unit);
  }
}

exports._getMessage = function(msg) {
  return function(just) {
    return function(nothing) {
      return function() {
        return Message.findOne({group: msg.group, seq: msg.seq})
          .exec()
          .then(function (m) {
            if (m) {
              return just(toJSON(m));
            }
            return nothing;
          });
      }
    }
  }
}

exports._getMessageList = function(group) {
  return function() {
    return Message.find({group: group})
      .exec()
      .then(function(msgs) {
        return msgs.map(toJSON)
      });
  }
}

exports._deleteMessage = function(msg) {
  return function() {
    return Message.findOneAndRemove({
      group: msg.group,
      seq: msg.seq
    })
      .exec()
      .then(unit);
  }
}

exports._subscribeMessage = function(m) {
  return function() {
    return Subscribe.findOne(m)
      .exec()
      .then(function(m0) {
        if (m0) {
          return m0;
        }
        return new Subscribe(m).save();
      })
      .then(unit);
  }
}

exports._unSubscribeMessage = function(m) {
  return function() {
    return Subscribe.findOneAndRemove(m).exec().then(unit);
  }
}

exports._getSubscribeList = function(group) {
  return function() {
    return Subscribe.find({group: group})
      .exec()
      .then(function(subList) {
        return subList.map(function(sub) {
          return sub.user;
        })
      });
  }
}

exports._roomSubscribeMessage = function(m) {
  return function() {
    return RoomSubscribe.findOne(m)
      .exec()
      .then(function(m0) {
        if (m0) {
          return m0;
        }
        return new RoomSubscribe(m).save();
      })
      .then(unit);
  }
}

exports._unRoomSubscribeMessage = function(m) {
  return function() {
    return RoomSubscribe.findOneAndRemove(m).exec().then(unit);
  }
}

exports._getRoomSubscribeList = function(group) {
  return function() {
    return RoomSubscribe.find({group: group})
      .exec()
      .then(function(subList) {
        return subList.map(function(sub) {
          return sub.room;
        })
      });
  }
}
