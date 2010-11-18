NB.CreateAppMessages = function(el)
{
    var _messageBanner = $(el);
    var _bannerVisible = false;

    var _messages = [];
    

    var _show = function(msg) {
        _mesages.push(msg);
        _messageBanner.html(msg);
        _messageBanner.removeClass('error');

        _bannerVisible = true;
    };

    var _showError = function(msg) {
        _message.push(msg);
        _messageBanner.addClass('error');
        if (!_bannerVisible)
            _messageBanner.slideDown('fast');

        _bannerVisible = true;
    };

    var _hide = function() { 
        _messageBanner.slideUp('fast');
        _bannerVisible = false;
    };



    return {
        getMessages: function() { return _messages },
        show: _show,
        showError : _showError,
        hide : _hide
    };
}