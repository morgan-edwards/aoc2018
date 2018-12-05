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

module.exports = {
  getLocalData,
  head,
  tail,
}
