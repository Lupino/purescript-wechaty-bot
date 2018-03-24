var periodic = require('node-periodic');

exports._newClient = function(options, poolOpts) {
  return function() {
    return new periodic.PeriodicClientPool(options, poolOpts);
  }
}

function mkfn2(fn) {
  return function(client) {
    return function(a) {
      return function(onError, onSuccess) {
        client[fn](a, function(err, ret) {
          if (err) {
            onError(err);
          } else {
            onSuccess(ret);
          }
        });

        // Return a canceler, which is just another Aff effect.
        return function (cancelError, cancelerError, cancelerSuccess) {
          cancelerSuccess(); // invoke the success callback for the canceler
        };
      }
    }
  }
}

function mkfn1(fn) {
  return function(client) {
    return function(onError, onSuccess) {
      client[fn](function(err, ret) {
        if (err) {
          onError(err);
        } else {
          onSuccess(ret);
        }
      });

      // Return a canceler, which is just another Aff effect.
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelerSuccess(); // invoke the success callback for the canceler
      };
    }
  }
}

exports._submitJob = mkfn2('submitJob');
exports._ping = mkfn1('ping');
exports._status = mkfn1('status');
exports._dropFunc = mkfn2('dropFunc');
exports._removeJob = mkfn2('removeJob');
