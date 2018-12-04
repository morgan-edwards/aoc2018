const axios = require('axios');
const fs = require('fs');

const getLocalData = (fileName) => (
  new Promise((resolve, reject) => {
    fs.readFile(fileName,'utf8', (err, data) => {
      if (err) {
        console.log(err);
        reject(err);
      }
      resolve(data);
    });
  })
);

getNetworkData = async (url) => {
  const data = await axios.get(url);
  console.log(data);
  return data;
}



const head = (arr) => {
  return arr[0];
};

const tail = (arr) => {
  return arr.slice(1, arr.length);
};

module.exports = {
  getLocalData,
  getNetworkData,
  head,
  tail,
}
