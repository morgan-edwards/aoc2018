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

const hour = (entry) => parseInt(time.slice(11, 13));
const minute = (entry) => parseInt(time.slice(14));

const sleepCycles = (entrySet) => {
  const cycles = [];
  let cycle = [];
  entrySet.forEach(e => {
    if (e.act === 'sleep') cycle = [{ h: hour(e.time), m: minute(e.time)}];
    if (e.act === 'wake') {
      cycle.push({ h: hour(e.time), m: minute(e.time)});
      cycles.push(cycle);
      cycle = [];
    }
  });
}

const makeShift = (entrySet) => {
  const hours = [];
  const cycles = sleepCycles(entrySet);
  let minute = 0;
  let currentCycle = cycles.shift();
  let guardState = currentCycle.h === 23 ? 's' : 'a';
  while (minute < 24) {
    if (hour > 23 && currentCycle.m > 0)
  }
};

const createAction = (data) => {
  const time = data.slice(1, 17);
  const guard = find(data, /#.[^ ]/);
  const act = guard ? 'start' : getAct(data);
  return { time, guard, act };
}

const shifts = (entries) => {
  const shiftsByGuard = {};
  let onDuty;
  let day;
  let sleep;
  let wake;
  entries.forEach(e => {
    if (e.act === 'start')
    day = e.time.slice(0, 10);
    if ()
  });
};

const processLog = (entries) => {
  const pEntries = entries.map(e => createAction(e));
  return pEntries.sort((a, b) => {
    if (a.time < b.time) return -1;
    return 1;
  })
}

const entries = dataLines(testData);
console.log(processLog(entries));
