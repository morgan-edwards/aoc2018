const fs = require('fs');
const { head, tail } = require('./utils.js');

const getData = () => (
  new Promise((resolve, reject) => {
    fs.readFile('./dayOneData.txt','utf8', (err, data) => {
      if (err) {
        console.log(err);
        reject(err);
      }
      resolve(data);
    });
  })
)

const parseData = (data) => {
  const blocks = data.split('\n');
  const actions = [];
  blocks.forEach(d => {
    const [symbol, amt] = [d.slice(0,1), d.slice(1)];
    if (symbol && amt) actions.push([symbol, amt]);
  });
  return actions;
}

const adjustFreq = (action, cf) => {
  if (action[0] === '-') return cf - (parseInt(action[1]));
  return cf + parseInt(action[1]);
}

const process = (actions) => {
  const total = actions.reduce((acc, act) => {
    return adjustFreq(act, acc);
  }, 0);
  return total;
}

const getFreq = async () => {
  const input = await getData();
  const actions = parseData(input);
  const freq = process(actions);
  console.log(freq);
}

const getRepeatFreq = async () => {
  const input = await getData();
  let actions = parseData(input);
  let currentFreq = 0;
  let tracked = new Set([]);
  let repeated = false;
  while (!repeated) {
    if (actions.length < 1) actions = parseData(input);
    const nextAct = head(actions);
    actions = tail(actions);
    currentFreq = adjustFreq(nextAct, currentFreq);
    if (tracked.has(currentFreq)) repeated = currentFreq;
    tracked.add(currentFreq);
    console.log(currentFreq);
  }
  console.log('FOUND');
  console.log(repeated);
}

getRepeatFreq();
