var _user$project$Native_WebAudioApi = function() {

    var AudioContext = window.AudioContext || window.webkitAudioContext;
    var context = new AudioContext();

    var unlock = function() {
        var buffer = context.createBuffer(1, 1, 22050);
        var source = context.createBufferSource();
        source.buffer = buffer;
        source.connect(context.destination);
        source.start(0);
        window.removeEventListener('touchend', unlock, true);
    };
    window.addEventListener('touchend', unlock, true);

    function decodeAudioData(arrayBuffer) {
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
            context.decodeAudioData(arrayBuffer, function(buffer) {
                callback(_elm_lang$core$Native_Scheduler.succeed(buffer));
            }, function(e) {
                callback(_elm_lang$core$Native_Scheduler.fail(e.toString()));
            });
        });
    }

    return {
        decodeAudioData: decodeAudioData,
    };

}();
