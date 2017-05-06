let AudioContext = window.AudioContext || window.webkitAudioContext;
let context = new AudioContext();
var unlock = function() {
  var buffer = context.createBuffer(1, 1, 22050);
  var source = context.createBufferSource();
  source.buffer = buffer;
  source.connect(context.destination);
  source.start(0);
  window.removeEventListener('touchend', unlock, true);
};
window.addEventListener('touchend', unlock, true);

var source = null;
app.ports.webAudioApiPlay.subscribe(data => {
  let buffer = data[0];
  let time = data[1];
  if (source) {
    source.stop(0);
    source = null;
  }
  source = context.createBufferSource();
  source.buffer = buffer;
  source.connect(context.destination);
  source.start(0, time);
  source.onended = function() {
    app.ports.mp3Finished.send({});
  }
});
app.ports.webAudioApiStop.subscribe(() => {
  if (source) {
    source.stop(0);
    source = null;
  }
});
