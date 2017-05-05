var fs = require('fs');
var Elm = require('./elm-gen-json.js');
var app = Elm.GenJson.worker();
app.ports.response.subscribe(json => {
  fs.writeFileSync('music.json', JSON.stringify(json, null, '  '));
  process.exit(0);
});
