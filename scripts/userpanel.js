NB.CreateAuthPanel = function(el, successCallback, failCallback) {
    var _panelEl =  el;
    var _authSuccess = successCallback;
    var _authHttpFail = failCallback;
    

    var _errorMessage = null;
    var _usernameIpt = null;
    var _serverSaltIpt = null;
    var _serverTimeIpt = null;
    var _passwdIpt = null;
    var _realNameIpt = null;
    var _passwdConfrmIpt = null;
    var _emailIpt = null;
    var _isRegister = null;
    var _registerAdditions = null;
    var _confirmButton = null;
    var _authSwitcher = null;
    


    var _createPassHash = function() {
        var pass = _passwdIpt.val();
        var salt = _serverSaltIpt.val();
        
        return hex_hmac_sha1(pass, salt);
    };
    
    var _createLoginHash = function() {
        var time = _serverTimeIpt.val();
        return hex_hmac_sha1(_createPassHash(), time);
    };
    


    var _submit = function(e) {
        var passHash = null;

        var data = {
            email: _emailIpt.val()
        };
        
        var action = "authenticate";
        
        if (_isRegister)
        {
            action = "account";
            data["passhash"] = _createPassHash();
            data["name"] = _realNameIpt.val();
        }
        else
        {
            data["passhash"] = _createLoginHash();
            data["timestamp"] = _serverTimeIpt.val();
        }
        

        $.ajax({
            type: 'POST',
            url: action,
            data: data,
            dataType: 'json',
            success: _authSuccess,
            error: _authHttpFail
        });
    }
 
    var _toggleMode = function(e) {
        if (_isRegister)
            _registerAdditions.slideUp('fast', function() { _confirmButton.val('sign in'); _authSwitcher.text("Not registered?"); });
        else
            _registerAdditions.slideDown('fast', function() { _confirmButton.val('register'); _authSwitcher.text("Already registered?"); });
            
        _isRegister = !_isRegister;
    };

    
    var _findAuthCtrls = function()
    {
        _errorMessage = _panelEl.find('.err-msg');

        _usernameIpt = _panelEl.find('.auth-user');
        _passwdIpt = _panelEl.find('.auth-pass');
        _serverSaltIpt = _panelEl.find('input[name=server_salt]');
        _serverTimeIpt = _panelEl.find('input[name=server_time]');
    
        _realNameIpt = _panelEl.find('.auth-realname');
        _passwdConfrmIpt = _panelEl.find('.auth-passconfirm');
        _emailIpt = _panelEl.find('.auth-email');
        _isRegister = _panelEl.hasClass("register-mode");
        _registerAdditions = _panelEl.find(".register-additions");
    
        _confirmButton = _panelEl.find('input[name=confirm]');
        _confirmButton.click(_submit);

        _authSwitcher = _panelEl.find('.auth-switcher');
        _authSwitcher.click(_toggleMode);
    }


    _findAuthCtrls();
    
    return {
        setServerTime : function(t) { _serverTimeIpt.val(t); }
    }
};


NB.CreateUserPanel = function(el) {
    var _userPanelEl = $(el);
    var _messageEl = _userPanelEl.find('.message');
    var _accountDetailsPanel = _userPanelEl.find(".account-details");
    var _signOutButton = _userPanelEl.find('.sign-out');
    var _authPanelEl = _userPanelEl.find(".auth-box");
    var _authPanel = null;
    
    var _isSignIn = true;

    
    var _userNameEl = _accountDetailsPanel.find(".user-name");
    var _noteCountEl = _accountDetailsPanel.find('.note-count');


    var _showMessage = function(message, isError) {
        _messageEl.html(message);
        if (isError) {
            if (!_messageEl.hasClass('error'))
                _messageEl.addClass('error');
        } else {
            _messageEl.removeClass('error');
        }
        
        _messageEl.slideDown('fast');
    };

    
    var _clearMessage = function() {
        _messageEl.slideUp('fast'); 
    };
    
    var _showUserDetails = function() {
        _authPanelEl.slideUp('fast');
        _accountDetailsPanel.slideDown('fast');
        _isSignIn = false;
    };

    
    var _showSignInPanel = function() {
        _accountDetailsPanel.slideUp('fast');
        _authPanelEl.slideDown('fast');
        _isSignIn = true;
    };
    

    var _netSuccess = function(data) {
        var state = data['state'];
        
        if (state === 'Active' || state === 'Tentative') {
            _userNameEl.text(data['name']);
            _showUserDetails();
        } else {
            if (state === 'Anon') {
                _showSignInPanel();
                if (data.hasOwnProperty('error')) {
                    if (data.hasOwnProperty('message'))
                        _showMessage(data['message']);
                    else
                        _showMessage(data['error']);
                }
            }
            else
            {
                _showMessage(state);
            }
        }

        if (data.hasOwnProperty('time'))
            _authPanel.setServerTime(data['time']);

        NB.App.AccountManager.update(data);
    };

    
    
    var _netFailure = function(req, status, exception) {
        _showMessage("Communication error");
    };

    
    
    var _update = function(action, method)
    {
        // TODO: Security check 'action'...
        if (!action)
            action = 'account';
            
        if (!method)
            method = 'GET';
            
        $.ajax({
            type: method,
            url: action,
            dataType: 'json',
            success: _netSuccess,
            error: _netFailure
        });
    }
    

    var _init = function()
    {
        _authPanel = NB.CreateAuthPanel(_authPanelEl, _netSuccess, _netFailure);
        _signOutButton.click(function() { _update('signout', 'POST'); });
    }
    
    _init();
    
    return {
        update: _update,
        showMessage: _showMessage,
        clearMessage: _clearMessage
    };
};