'use strict';

require('./index.html');
require('./global.css');
require('./Stylesheets');

// set up db
var Dexie = require('dexie');
var db = new Dexie('cuber');
db.version(1).stores({
  solves: '++id,start,solveTime',
});

// set up elm
var Elm = require('./Main');
var app = Elm.Main.embed(document.querySelector('#main'));

// set up ports
app.ports.requestLatestSolves.subscribe(function () {
    findLatestSolves()
      .then(sendSolves);
});

app.ports.createSolve.subscribe(function (solve) {
  db.solves
    .add(solve)
    .then(findLatestSolves)
    .then(sendSolves);
});

app.ports.requestDeleteSolve.subscribe(function (id) {
  db.solves
    .delete(id)
    .then(findLatestSolves)
    .then(sendSolves);
});

// helper functions
function findLatestSolves(limit = 200) {
  const truncated = limit > 200 ? 200 : limit;
  return db.solves
    .orderBy('start')
    .reverse()
    .limit(limit)
    .toArray();
}

function sendSolves(solves) {
  app.ports.setLatestSolves.send(solves);
}
