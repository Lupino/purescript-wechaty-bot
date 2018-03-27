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

var UserSchema = new Schema({
  userid:     {type: String, index: {unique: true}},
  name:       {type: String, index: true},
  created_at: {type: Number, default: timestamp},
});

var User = mongoose.model('User', UserSchema);

var MessageSchema = new Schema({
  userid:     {type: String, index: true},
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
  userid:     {type: String, index: true},
  group:      {type: String, index: true},
  created_at: {type: Number, default: timestamp},
});

SubscribeSchema.plugin(autoIncrPlugin, { model: 'Subscribe', field: 'id', startAt: 1 });

var Subscribe = mongoose.model('Subscribe', SubscribeSchema);

function toJSON(u) {
  if (u.toJSON) {
    return u.toJSON();
  }
  return u;
}

function unit() {
  return {}
}

exports._saveUser = function(u) {
  return function() {
    return User.findOne({userid: u.userid})
      .exec()
      .then(function(u0) {
        if (u0) {
          return u0;
        }
        return new User({userid: u.userid, name: u.name}).save();
      })
      .then(unit);
  }
}

exports._getUser = function(userid) {
  return function(just) {
    return function(nothing) {
      return function() {
        return User.findOne({userid: userid})
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

exports._createMessage = function(msg) {
  return function() {
    return new Message({
      userid: msg.userid,
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
          return sub.userid;
        })
      });
  }
}
