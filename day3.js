const {
  getLocalData,
  getNetworkData,
  head,
  tail,
} = require('./utils.js');

const processItem = (item) => {
  const elArr = item.split(' ');
  const [id, _, loc, dim] = elArr;
  const [column, row] = loc.split(',');
  const [width, height] = dim.split('x');
  return {
    id,
    column: parseInt(column),
    row: parseInt(row.slice(0, row.length)),
    width: parseInt(width),
    height: parseInt(height),
  }
}

// const fabric = Array(8).fill(Array(8).fill('.'));
const fabric = Array(1000).fill(Array(1000).fill('.'));

const testData = `#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2`

const cutCols = (row, cut) => {
  const from = cut.column;
  const to = cut.column + cut.width;
  return row.map((col, idx) => {
    if (idx >= from && idx < to) {
      if (col !== '.') return 'X';
      return cut.id;
    }
    return col;
  });
}

const makeCut = (fabric, cut) => {
  const from = cut.row;
  const to = cut.row + cut.height;
  return fabric.map((row, idx) => {
    if (idx >= from && idx < to) {
      return cutCols(row, cut);
    }
    return row;
  })
}

const makeAllCuts = (fabric, cuts) => {
  if (cuts.length === 0) return fabric;
  const nextCut = head(cuts);
  const remaining = tail(cuts);
  return makeAllCuts(makeCut(fabric, nextCut), remaining);
}

const conflicts = (fabric, count) => {
  if (fabric.length === 0) return count;
  const row = head(fabric);
  const conCount = row.reduce((acc, col) => {
    if (col === 'X') acc += 1;
    return acc;
  }, 0);
  return conflicts(tail(fabric), count + conCount);
}

const cutConflicts = (cutFabric, cut) => {
  const { id } = cut;
  const fullCut = cut.width * cut.height;
  const actualAmt = cutFabric.reduce((acc1, row) => {
    return acc1 + row.reduce((acc2, col) => {
      if (col === id) return acc2 + 1;
      return acc2;
    }, 0);
  }, 0);;
  return { size: fullCut, filled: actualAmt };
};

const fullCuts = (cutFabric, cuts) => {
  const execCuts = cuts.map(cut => Object.assign(
    {},
    cut,
    cutConflicts(cutFabric, cut),
  ));
  return execCuts.reduce((acc, ec) => {
    if (ec.size === ec.filled) acc.push(ec);
    return acc;
  }, []);
}

const doTest = () => {
  const cuts = testData.trim().split('\n').map(i => processItem(i));
  const cutFabric = makeAllCuts(fabric, cuts);
  const fullcuts = fullCuts(cutFabric, cuts);
  console.log(fullcuts);
}

// doTest();

getLocalData('dayThreeData.txt').then(input => {
  const cuts = input.trim().split('\n').map(i => processItem(i));
  const cutFabric = makeAllCuts(fabric, cuts);
  // const cons = conflicts(cutFabric, 0);
  const full = fullCuts(cutFabric, cuts);
  console.log(full);
});
