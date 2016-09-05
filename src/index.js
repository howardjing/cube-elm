'use strict';

require('./index.html');
require('./Stylesheets');

var Elm = require('./Main');

Elm.Main.embed(document.querySelector('#main'));