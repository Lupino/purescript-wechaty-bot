const Sequelize = require('sequelize');

exports.connect = function(dsn) {
  return function(opts) {
    return function() {
      return new Sequelize(dsn, opts);
    };
  };
};

exports.connectOpts = function(opts) {
  return function() {
    return new Sequelize(opts);
  };
};

exports._query = function(sequelize) {
  return function(q) {
    return function(opts) {
      return function() {
        opts.type = opts.type || sequelize.QueryTypes.SELECT;
        return sequelize.query(q, opts);
      };
    };
  };
};

/* eslint new-cap: "off"*/

exports.define = function(sequelize) {
  return function(name) {
    return function(struct) {
      return function(opts) {
        return sequelize.define(name, struct, opts);
      };
    };
  };
};

exports.string = Sequelize.STRING;
exports.text = Sequelize.TEXT;
exports.json = Sequelize.JSON;
exports.uint = function(s) {
  return Sequelize.INTEGER(s).UNSIGNED;
};

exports.int = Sequelize.INTEGER;
exports.bigint = Sequelize.BIGINT;

exports.timestamp = function() {
  return Math.floor(new Date() / 1000);
};

function mapToJson(just, nothing) {
  return function(o) {
    if (o) {
      return just(toJSON(o));
    }
    return nothing;
  };
}

function toJSON(o) {
  if (o && o.toJSON) {
    o = o.toJSON();
  }
  return o;
}

function arrMapToJson(arr) {
  return arr.map(toJSON);
}

exports._findOne = function(model) {
  return function(just) {
    return function(nothing) {
      return function(opts) {
        return function() {
          return model.findOne(opts)
            .then(mapToJson(just, nothing));
        };
      };
    };
  };
};

exports._update = function(model) {
  return function(updated) {
    return function(opts) {
      return function() {
        return model.update(updated, opts);
      };
    };
  };
};

exports._create = function(model) {
  return function(obj) {
    return function() {
      return model.create(obj).then(toJSON);
    };
  };
};

exports._sync = function(model) {
  return function(opts) {
    return function() {
      return model.sync(opts);
    };
  };
};

exports._destory = function(model) {
  return function(opts) {
    return function() {
      return model.destroy(opts);
    };
  };
};

exports._findAndCountAll = function(model) {
  return function(opts) {
    return function(tuple) {
      return function() {
        return model.findAndCountAll(opts).then(function(ret) {
          return tuple(ret.count)(ret.rows.map(toJSON));
        });
      };
    };
  };
};

exports._findOrCreate = function(model) {
  return function(opts) {
    return function() {
      return model.findOrCreate(opts).then(function(o) {
        return toJSON(o[0].dataValues);
      });
    };
  };
};

exports._count = function(model) {
  return function(opts) {
    return function() {
      return model.count(opts);
    };
  };
};

exports._decrement = function(model) {
  return function(fields) {
    return function(opts) {
      return function() {
        return model.decrement(fields, opts);
      };
    };
  };
};

exports._increment = function(model) {
  return function(fields) {
    return function(opts) {
      return function() {
        return model.increment(fields, opts);
      };
    };
  };
};

exports._findAll = function(model) {
  return function(opts) {
    return function() {
      return model.findAll(opts).then(arrMapToJson);
    };
  };
};

exports._upsert = function(model) {
  return function(values) {
    return function(opts) {
      return function() {
        return model.upsert(values, opts);
      };
    };
  };
};

exports._sum = function(model) {
  return function(field) {
    return function(opts) {
      return function() {
        return model.sum(field, opts);
      };
    };
  };
};

exports.getTableName = function(model) {
  return model.getTableName();
};

exports.hasOne = function(model) {
  return function(target) {
    return function(opts) {
      return function() {
        return model.hasOne(target, opts);
      };
    };
  };
};

exports.hasMany = function(model) {
  return function(target) {
    return function(opts) {
      return function() {
        return model.hasMany(target, opts);
      };
    };
  };
};
