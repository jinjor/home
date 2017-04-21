var _user$project$Native_WebAudioApi = function() {

var AudioContext = window.AudioContext || window.webkitAudioContext;
var context = new AudioContext();

function decodeAudioData(arrayBuffer) {
  return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback){
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
