const moment = require('moment');

const {
  getLocalData,
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

const testSet = [{ time: '1518-11-01 00:00', guard: '#10', act: 'start' },
  { time: '1518-11-01 00:05', guard: null, act: 'sleep' },
  { time: '1518-11-01 00:25', guard: null, act: 'wake' },
  { time: '1518-11-01 00:30', guard: null, act: 'sleep' },
  { time: '1518-11-01 00:55', guard: null, act: 'wake' }]

const find = (string, regex) => {
  const res = string.match(regex) ? string.match(regex)[0] : null;
  return res;
};

const dataLines = (data) => {
  return data.trim().split('\n');
};

const getAct = (data) => {
  if (/asleep/.test(data)) return 'sleep';
  if (/wakes/.test(data)) return 'wake';
  return '';
};

const hour = (e) => parseInt(e.time.slice(11, 13));
const minute = (e) => parseInt(e.time.slice(14));
const date = (e) => {
  if (hour(e) > 0) {
    return moment(e.time.slice(0, 10)).add(1, 'day').format('YYYY-MM-DD');
  }
  return e.time.slice(0,10);
}

const sleepCycles = (entrySet) => {
  const cycles = [];
  let cycle = [];
  entrySet.forEach(e => {
    if (e.act === 'sleep') cycle = [{ h: hour(e), m: minute(e)}];
    if (e.act === 'wake') {
      cycle.push({ h: hour(e), m: minute(e)});
      cycles.push(cycle);
      cycle = [];
    }
  });
  return cycles;
}

const makeShift = (entrySet) => {
  const hour = [];
  const cycles = sleepCycles(entrySet);
  let minute = 0;
  let [sleep, wake] = cycles.shift();
  let guardState = 'awake';
  while (minute < 60) {
    if (minute === sleep.m) guardState = 'asleep';
    if (minute === wake.m) {
      [sleep, wake] = cycles[0] || [61, 61];
      guardState = 'awake';
      cycles.shift();
    }
    hour.push(guardState);
    minute += 1;
  }
  return hour;
};

const createAction = (data) => {
  const time = data.slice(1, 17);
  const guard = find(data, /#.[^ ]/);
  const act = guard ? 'start' : getAct(data);
  return { time, guard, act };
}

const nights = (data) => {
  const nights = {};
  let guard = data[0].guard;
  let currDate = date(data[0]);
  let entries = [];
  data.forEach(e => {
    const now = date(e);
    if (currDate !== now) {
      nights[currDate] = { guard, entries };
      guard = e.guard;
      currDate = now;
      entries = [e];
    } else {
      entries.push(e);
    }
  });
  nights[currDate] = { guard, entries };
  return nights;
}

const processLog = (entries) => {
  const pEntries = entries.map(e => createAction(e));
  return pEntries.sort((a, b) => {
    if (a.time < b.time) return -1;
    return 1;
  })
}

const firstSolution = (raw) => {
  const data = processLog(dataLines(raw));
  const shiftData = nights(data);
  const withHours = Object.keys(shiftData).map(key => {
    const sd = shiftData[key];
    const hours = makeShift(sd.entries);
    const slept = hours.filter(h => h === 'asleep').length
    return Object.assign({}, sd, { hours , slept, date: key });
  });
  const guardSleep = withHours.reduce((acc, shift) => {
    const tracker = acc[shift.guard] || { totalSlept: 0, hours: {} };
    tracker.totalSlept += shift.slept;
    shift.hours.forEach((h, idx) => {
      let hour = tracker.hours[idx] || 0;
      if (h === 'asleep') hour += 1;
      tracker.hours[idx] = hour;
    });
    acc[shift.guard] = tracker;
    return acc;
  }, {});
  const sleepiest = Object.keys(guardSleep).map(key => Object.assign({}, guardSleep[key], { guard: parseInt(key.slice(1)) }))
    .sort((a, b) => {
      if (a.totalSlept < b.totalSlept) return 1;
      return -1;
    })[0];
  const sleepiestHour = Object.keys(sleepiest.hours).map(key => ({ h: key, amt: sleepiest.hours[key] }))
    .sort((a, b) => {
      if (a.amt < b.amt) return 1;
      return -1;
    })[0];
  return parseInt(sleepiestHour.h) * sleepiest.guard;
};

getLocalData('./data/dayFourData.txt').then(res => {
  const solutionOne = firstSolution(testData);
  console.log('FIRST: ', solutionOne);
});
