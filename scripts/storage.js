NB.CreateStorageManager = function() {
    var _write = function(path, content, successCallback, failureCallback) {
        $.ajax({
            type: "POST",
            url: path,
            data: content,
            success: successCallback,
            error: failureCallback,
            dataType: "json"
        });

        localStorage[path] = JSON.stringify(content);
    };


    var _read = function(path, successCallback, failureCallback) {
        $.ajax({
            type: "GET",
            url: path,
            success: successCallback,
            error: failureCallback,
            dataType: 'json'
        });
    };

    var _createDelayedSaver =  function(path, delay, getContent, success, fail) {
        var _path = path;
        var _delay = delay;
        var _successCallback = success;
        var _failCallback = fail;
        
        var _timer = null;
        
        var _doSave = function() {
            var content = getContent();
            if (content === null)
                return;
            
            _write(_path, content, _successCallback, _failCallback);
        };
        
        var _queueSave = function(e) {
            if (_timer)
                clearTimeout(_timer);
            
            _timer = setTimeout(_doSave, delay);
        };
        
        var _cancelCurrent = function() {
            if (_timer)
                clearTimeout(_timer);
            
            _timer = null;
        };
        
        var _setPath = function(newPath) {
            if (_timer)
                clearTimeout(_timer);
            
            _path = newPath;
        };
        
        
        return {
            queueSave: _queueSave,
            cancelCurrent: _cancelCurrent,
            setPath: _setPath
        };
    }
    
    return {
        createDelayedSaver: _createDelayedSaver,
        read: _read
    };
}