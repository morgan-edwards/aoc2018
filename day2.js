const {
  getLocalData,
  getNetworkData,
  head,
  tail,
} = require('./utils.js');

const testData = [
  'abcdef',
  'bababc',
  'abbcde',
  'abcccd',
  'aabcdd',
  'abcdee',
  'ababab',
];

const testCheckSum = 12;

const processId = (id) => {
  const idArr = id.split('');
  const letterCounts = idArr.reduce((acc, chr) => {
    if (acc[chr]) {
      acc[chr] += 1;
      return acc;
    }
    acc[chr] = 1;
    return acc;
  }, {});
  const twoThrees = Object.values(letterCounts).reduce((acc, val)=> {
    if (val === 2) acc.two += 1;
    if (val === 3) acc.three += 1;
    return acc;
  }, { two: 0, three: 0 });
  return twoThrees;
}

const getChecksum = (ids) => {
  const totals = ids.reduce((acc, id) => {
    const adjustment = processId(id);
    console.log(id, adjustment);
    if (adjustment.two > 0) acc.two += 1;
    if (adjustment.three > 0) acc.three += 1;
    return acc;
  }, { two: 0, three: 0 });
  return Object.values(totals).reduce((acc, val) => acc * val);
}

const removeChar = (idx, id) => (
  id.slice(0, idx).concat(id.slice(idx + 1, idx.length))
)

// const getCommonLetters = (ids) => {
//   const seen = new Set([]);
//   const edited = {};
//   ids.forEach(id => {
//     id.split('').forEach((_, idx) => {
//       const newId = removeChar(idx, id);
//       if (edited[newId]) {
//         edited[newId] += 1;
//       } else {
//         edited[newId] = 1;
//       }
//     });
//   });
//   const common = Object.keys(edited).reduce((acc, id) => {
//     if (edited[id] > 1) acc.push(id);
//     return acc;
//   }, []);
//   console.log(common);
// }

const getCommonLetters = (ids) => {
  let common = [];
  let iter = 0;
  while (iter < ids[0].length) {
    const edits = new Set([]);
    ids.forEach(id => {
      const newId = removeChar(iter, id);
      console.log(newId);
      if (edits.has(newId)) common.push(newId);
      edits.add(newId);
    });
    iter += 1;
  }
  console.log(common);
}

getLocalData('dayTwoData.txt').then((data) => {
  const ids = data.trim().split('\n');
  // const checksum = getChecksum(ids);
  // console.log('CHECKSUM', checksum);
  getCommonLetters(ids);
});

// console.log(checkSum(testData));
