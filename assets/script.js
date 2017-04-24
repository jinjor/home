window.onerror = (message, url, line) => {
  document.getElementById('errors').textContent = message + '\n'
    + url + '\n' + line;
  return false;
};
let app = Elm.Main.fullscreen();
let AudioContext = window.AudioContext || window.webkitAudioContext;
let context = new AudioContext();
let sources = {};
app.ports.webAudioApiPlay.subscribe(data => {
  let id = data[0];
  let buffer = data[1];
  let time = data[2];
  let source = context.createBufferSource();
  source.buffer = buffer;
  source.connect(context.destination);
  source.start(0, time);
  sources[id] = source;
});
app.ports.webAudioApiStop.subscribe(id => {
  let source = sources[id];
  if(source) {
    source.stop(0);
    sources[id] = null;
  }
});
