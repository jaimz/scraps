NB.CreateStatusManager = function() {
    var _warnEv = "status warning";
    var _errorEv = "status error";

    
    var _dispatch = function(type, text) {
        NB.App.NotificationCenter.publish(type, null, this);
    };

    var _showWarning = function(warning) {
        console.warn(warning);
        _dispatch(_warnEv, warning);
    };
    
    var _showError = function(error) {
        console.error(error);
        _dispatch(_errorEv, error);
    };
    
    return {
        ShowError: _showError,
        ShowWarning: _showWarning
    }
};