NB.CreateAccountManager = function() {
    var _current = null;
    
    var _update = function(accountData) {
        if (!accountData)
        {
            console.warn("AccountManager: update given null account data");
            _current = null;
            return;
        }
        
        
        var oldState = null;
        if (_current && _current.hasOwnProperty("state"))
            oldState = _current["state"];
            
        var newState = null;
        if (accountData.hasOwnProperty("state"))
            newState = accountData["state"];
            
        _current = accountData;
        
        console.log('oldstate: ' + oldState);
        console.log('newState: ' + newState);
        
        if (oldState != newState)
            NB.App.NotificationCenter.publish("account changed", null);
    };
    
    var _currentAccount = function () { return _current; };

    
    return {
        update: _update,
        currentAccount: _currentAccount
    };
};