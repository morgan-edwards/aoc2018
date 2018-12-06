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
  if (/asleep/.test(data)) return 'sleep';
  if (/wakes/.test(data)) return 'wake';
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

// Parsing logic
const shifts = (log) => {
  const makeShift = (line) => ({
    date: date(line),
    guard: line.guard,
    hours: Array(60).fill(false),
    entries: [],
  });
  let currShift = makeShift(log[0]);
  let remainingLog = log.slice(1);
  const allShifts = remainingLog.reduce((acc, line) => {
    if (line.guard) {
      acc.push(currShift);
      currShift = makeShift(line);
      return acc;
    };
    currShift.entries.push(line);
    return acc;
  }, []);
  allShifts.push(currShift);
  console.log(allShifts.slice(allShifts.length - 1)[0].entries);
  return allShifts;
};

const processHours = (hours, entries) => {
  const sleepPoints = entries.map(e => minute(e));
  if (sleepPoints.length === 0) sleepPoints.push(0);
  const toggleHours = (sps, hs, lowerBound) => {
    if (sps.length === 0) return hs;
    const nextPoint = head(sps);
    const remaining = tail(sps);
    const keep = hs.slice(0, lowerBound);
    const toggled = hs.map((h, idx) => {
      if (idx >= nextPoint) {
        return !h;
      }
      return h;
    });
    return toggleHours(remaining, toggled, nextPoint);
  }
  return toggleHours(sleepPoints, hours, 0);
};

const shiftsByGuard = (shiftData) => {
  return shiftData.reduce((acc, sd) => {
    const guardShifts = acc[sd.guard] || [];
    guardShifts.push(sd);
    acc[sd.guard] = guardShifts;
    return acc;
  }, {});
};

const hourBreakDowns = (shiftsByGuard) => {
  return Object.keys(shiftsByGuard).map(gId => {
    const hourTotals = shiftsByGuard[gId].reduce((acc, sd) => {
      const { hours } = sd;
      hours.forEach((h, idx) => {
        if (h) acc[idx] = (acc[idx] || 0) + 1;
      });
      return acc;
    }, {});
    return { guardId: gId, hours: hourTotals };
  });
};

const addHours = (hourBreakDowns) => {
  return hourBreakDowns.map(hbd => {
    const total = Object.values(hbd.hours).reduce((acc, amt) => acc + amt);
    const mostSlept = Object.keys(hbd.hours).map(key => ({ hour: key, amt: hbd.hours[key] }))
      .sort((a, b) => {
        if (a.amt < b.amt) return 1;
        return -1;
      })[0];
    return Object.assign({}, hbd, { total, mostSlept });
  });
};

// First solution
const firstSolution = (raw) => {
  const log = processLog(dataLines(raw));
  const shiftData = shifts(log).map(s => Object.assign({}, s, { hours: processHours(s.hours, s.entries) }));
  const guardShifts = shiftsByGuard(shiftData);
  console.log(guardShifts['#22']);
  const breakdowns = hourBreakDowns(guardShifts);
  const totals = addHours(breakdowns);
  const worstGuard = totals.sort((a, b) => {
    if (a.total < b.total) return 1;
    return -1;
  })[0];
  return parseInt(worstGuard.guardId.slice(1)) * parseInt(worstGuard.mostSlept.hour);
};

getLocalData('./data/dayFourData.txt').then(res => {
  const solutionOne = firstSolution(res);
  console.log('FIRST: ', solutionOne);
});
