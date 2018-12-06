const moment = require('moment');

const {
  getLocalData,
  head,
  tail,
} = require('./utils/utils.js');

const testData = `
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-04 00:36] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-01 00:05] falls asleep
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
`

// Processing functions
const dataLines = (data) => {
  const lines = data.trim().split('\n');
  return lines;
};

const processLog = (entries) => {
  const pEntries = entries.map(e => createAction(e));
  return pEntries.sort((a, b) => {
    if (a.time < b.time) return -1;
    return 1;
  })
}

const find = (string, regex) => {
  const res = string.match(regex) ? string.match(regex)[0] : null;
  return res;
};

const getAct = (data) => {
  if (/asleep/.test(data)) return 'asleep';
  if (/wakes/.test(data)) return 'awake';
  return '';
};

const createAction = (data) => {
  const time = data.slice(1, 17);
  const guard = find(data, /#.[^ ]/);
  const act = guard ? 'start' : getAct(data);
  return { time, guard, act };
}

const hour = (e) => parseInt(e.time.slice(11, 13));
const minute = (e) => parseInt(e.time.slice(14, 16));
const date = (e) => {
  if (hour(e) > 0) {
    return moment(e.time.slice(0, 10)).add(1, 'day').format('YYYY-MM-DD');
  }
  return e.time.slice(0,10);
}

const succ = (time) => {
  return moment(time).add(1, 'minutes').format('YYYY-MM-DD HH:mm');
}

// Storage vars
let currentGuard;
let time;
let state = 'wake';
const allMinutes = [];

// Soltion Logic
const firstSolution = (data) => {
  const lines = dataLines(data);
  const sortedEntries = processLog(lines);
  const entryMap = sortedEntries.reduce((acc, e) => {
    acc[e.time] = e;
    return acc;
  }, {});
  const entryTimes = Object.keys(entryMap);
  time = head(entryTimes);
  const finish = entryTimes.slice(entryTimes.length - 1)[0];
  while (time <= finish) {
    if (hour({ time }) !== 23 || hour({ time }) !== 0) {
      const entry = entryMap[time];
      if (entry) {
        if (entry.act === 'start') currentGuard = entry.guard;
        if (entry.act !== 'start') state = entry.act;
      };
      allMinutes.push({ guard: currentGuard, minute: minute({ time }), state });
    }
    time = succ(time);
  }
  const sleepingMinutes = allMinutes.filter(m => (m.state === 'asleep'));
  const minutesByGuard = sleepingMinutes.reduce((acc, item) => {
    const guardData = acc[item.guard] || [];
    guardData.push(item);
    acc[item.guard] = guardData;
    return acc;
  }, {});
  const guardReports = Object.keys(minutesByGuard).map(gId => {
    const minutes = minutesByGuard[gId];
    const total = minutes.length;
    const breakDown = minutes.reduce((acc, m) => {
      currentTotal = acc[m.minute] || 0;
      currentTotal += 1;
      acc[m.minute] = currentTotal;
      return acc;
    }, {});
    return ({ guard: gId, total, breakDown });
  });
  const sleepiestGuard = guardReports.sort((a, b) => {
    if (a.total < b.total) return 1;
    return -1;
  })[0];
  const withSleepiestHour = Object.assign({}, sleepiestGuard, {
    mostSlept: Object.keys(sleepiestGuard.breakDown).map(m => ({ minute: parseInt(m), amt: sleepiestGuard.breakDown[m] })).sort((a, b) => {
      if (a.amt < b.amt) return 1;
      return -1;
    })[0]
  });
  console.log('reports: ', withSleepiestHour);
  return parseInt(withSleepiestHour.guard.slice(1)) * withSleepiestHour.mostSlept.minute;
}

getLocalData('./data/dayFourData.txt').then(res => {
  const solutionOne = firstSolution(res);
  console.log('FIRST: ', solutionOne);
});
