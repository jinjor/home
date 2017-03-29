window.onerror = (message, url, line) => {
  document.getElementById('errors').textContent = message + '\n'
    + url + '\n' + line;
  return false;
};
let app = Elm.Main.fullscreen();
app.ports.start.subscribe(() => {
});
app.ports.stop.subscribe(() => {
});
