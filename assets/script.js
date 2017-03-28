window.onerror = (message, url, line) => {
  document.getElementById('errors').textContent = message + '\n'
    + url + '\n' + line;
  return false;
};
let sample = './assets/sample.mid';
let app = Elm.Main.fullscreen(sample);
app.ports.start.subscribe(() => {
});
app.ports.stop.subscribe(() => {
});
