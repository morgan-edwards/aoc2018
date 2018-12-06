const moment = require('moment');

const {
  getLocalData,
  head,
  tail,
} = require('./utils/utils.js');

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

const hour = (e) => parseInt(e.time.slice(11, 13));
const minute = (e) => parseInt(e.time.slice(14, 16));
const date = (e) => {
  if (hour(e) > 0) {
    return moment(e.time.slice(0, 10)).add(1, 'day').format('YYYY-MM-DD');
  }
  return e.time.slice(0,10);
}

const createAction = (data) => {
  const time = data.slice(1, 17);
  const guard = find(data, /#\S*/);
  const act = guard ? 'start' : getAct(data);
  return { time, guard, act };
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

// Solutions
const inputToGuards = (raw) => {
  const log = processLog(dataLines(raw));
  const shiftData = shifts(log).map(s => Object.assign({}, s, { hours: processHours(s.hours, s.entries) }));
  const guardShifts = shiftsByGuard(shiftData);
  const breakdowns = hourBreakDowns(guardShifts);
  return addHours(breakdowns);
};

const firstSolution = (raw) => {
  const guards = inputToGuards(raw);
  const worstGuard = guards.sort((a, b) => {
      if (a.total < b.total) return 1;
      return -1;
    })[0];
  return parseInt(worstGuard.guardId.slice(1)) * parseInt(worstGuard.mostSlept.hour);
};

const secondSolution = (raw) => {
  const guards = inputToGuards(raw);
  const consistent = guards.sort((a, b) => {
    if (a.mostSlept.amt < b.mostSlept.amt) return 1;
    return -1;
  })[0];
  return parseInt(consistent.guardId.slice(1)) * parseInt(consistent.mostSlept.hour);
}

getLocalData('./data/dayFourData.txt').then(res => {
  const solutionOne = firstSolution(res);
  const solutionTwo = secondSolution(res);
  console.log('FIRST: ', solutionOne);
  console.log('SECOND: ', solutionTwo);
});
