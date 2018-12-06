const fs = require('fs');

const getLocalData = (fileName) => (
  new Promise((resolve, reject) => {
    fs.readFile(fileName,'utf8', (err, data) => {
      if (err) {
        console.log(err);
        reject(err);
      }
      resolve(data.trim());
    });
  })
);

const head = (arr) => {
  return arr[0];
};

const tail = (arr) => {
  return arr.slice(1, arr.length);
};

const last = (arr) => arr.slice(arr.length - 1)[0];
const init = (arr) => arr.slice(0, arr.length - 1);

module.exports = {
  getLocalData,
  head,
  tail,
  last,
  init,
}
