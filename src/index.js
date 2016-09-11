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
    latestSolves()
      .then(function (solves) {
        app.ports.setLatestSolves.send(solves);
      });
});

app.ports.createSolve.subscribe(function (solve) {
  db.solves
    .add(solve)
    .then(function () {
      return latestSolves();
    })
    .then(function (solves) {
      app.ports.setLatestSolves.send(solves);
    });
});

function latestSolves() {
  return db.solves
    .orderBy('start')
    .reverse()
    .limit(10)
    .toArray();
}
