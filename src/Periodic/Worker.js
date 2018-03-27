var periodic = require('node-periodic');

exports.newWorker = function(options) {
  return function() {
    return new periodic.PeriodicWorker(options);
  }
}

exports._work = function(worker) {
  return function(size) {
    return function() {
      return worker.work(size);
    }
  }
}

exports._addFunc = function(worker) {
  return function(func) {
    return function(task) {
      return function() {
        return worker.addFunc(func, function(job) {
          task(job)();
        });
      }
    }
  }
}

exports._done = function(job) {
  return function() {
    return job.done();
  }
}

exports._fail = function(job) {
  return function() {
    return job.fail();
  }
}

exports._schedLater = function(job) {
  return function(delay) {
    return function() {
      return job.schedLater(delay)
    }
  }
}

exports._funcName = function(job) {
  return function() {
    return job.funcName;
  }
}

exports._name = function(job) {
  return function() {
    return job.name;
  }
}

exports._workload = function(job) {
  return function() {
    return job.workload;
  }
}
