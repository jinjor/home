window.onerror = (message, url, line) => {
    document.getElementById('errors').textContent = message + '\n' +
        url + '\n' + line;
    return false;
};
let app = Elm.Main.fullscreen();
let AudioContext = window.AudioContext || window.webkitAudioContext;
let context = new AudioContext();
var unlock = function() {
    var buffer = context.createBuffer(1, 1, 22050);
    var source = context.createBufferSource();
    source.buffer = buffer;
    source.connect(context.destination);
    source.start(0);
    window.removeEventListener('touchend', unlock, false);
};
window.addEventListener('touchend', unlock, false);

var source = null;
app.ports.moveToCard.subscribe(hash => {
    var element = document.getElementById(hash.substring(1));
    if (element) {
        var rect = element.getBoundingClientRect();
        setTimeout(() => {
            window.scrollTo(0, window.pageYOffset + rect.top);
        });
    }
});
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
});
app.ports.webAudioApiStop.subscribe(() => {
    if (source) {
        source.stop(0);
        source = null;
    }
});
