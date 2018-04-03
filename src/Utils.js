var moment = require('moment');

exports.startsWith = function(s) {
  return function(s1) {
    return s.startsWith(s1);
  }
}

exports.convertSchedAt = function(m) {
  var month = Number(m.month);
  var date = Number(m.date);
  var hour = Number(m.hour);
  var minute = Number(m.minute);

  var now = new Date();
  if (month < now.getMonth() + 1) {
    now.setYear(now.getFullYear() + 1);
  }
  now.setMonth(month - 1);
  now.setDate(date);
  now.setHours(hour);
  now.setMinutes(minute);
  return Math.floor(now / 1000);
}

exports.momentFormat = function(ts) {
  return function(tp) {
    return moment(ts * 1000).format(tp);
  }
}

var tsD = 24 * 60 * 60;
var tsH = 60 * 60;
var tsM = 60;

exports.formatTimeString = function(t) {
  if (t >= tsD) {
    return Math.floor(t / tsD) + 'd ' + exports.formatTimeString(t % tsD);
  } else if (t >= tsH) {
    return Math.floor(t / tsH) + 'h ' + exports.formatTimeString(t % tsH);
  } else if (t >= tsM) {
    return Math.floor(t / tsM) + 'm ' + exports.formatTimeString(t % tsM);
  } else if (t > 0) {
    return t + 's';
  } else {
    return '';
  }
}

exports.readNumber = function(n) {
  return Number(n) || 0;
}
