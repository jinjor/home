window.onerror = (message, url, line) => {
  document.getElementById('errors').textContent = message + '\n'
    + url + '\n' + line;
  return false;
};
let app = Elm.Main.fullscreen();
let AudioContext = window.AudioContext || window.webkitAudioContext;
let context = new AudioContext();
var resume = function () {
  context.resume();
  setTimeout(function () {
    if (context.state === 'running') {
      document.body.removeEventListener('touchend', resume, false);
    }
  }, 0);
};
document.body.addEventListener('touchend', resume, false);

var source = null;

app.ports.webAudioApiPlay.subscribe(data => {
  let buffer = data[0];
  let time = data[1];
  if(source) {
    source.stop(0);
    source = null;
  }
  source = context.createBufferSource();
  source.buffer = buffer;
  source.connect(context.destination);
  source.start(0, time);
});
app.ports.webAudioApiStop.subscribe(() => {
  if(source) {
    source.stop(0);
    source = null;
  }
});
