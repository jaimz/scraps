NB.CreateNotificationCenter = function() {
    var _keys = [];
    var _map = {};
    
    var _haveName = function(name) {
        var l = _keys.length;
        var haveKey = false;

        if (l > 0)
        {
            for (var i = 0; i < l; ++i)
            {
                haveKey = _keys[i] == name;
                if (haveKey)
                    break;
            }
        }
        
        return haveKey;
    };
    
    var _subscribe = function(name, f) {
        var contained = _haveName(name);

        var l = null;
        if (contained == false)
        {
            l = [];
            _keys.push(name);
            _map[name] = l;
        }
        else
        {
            console.log(_map);
            if (_map.hasOwnProperty(name)) {
                l = _map[name];
            } else {
                // shouldn't happen.
                console.warn("NotificationCenter: key seems to have disappeared: " + name);
                l = [];
                _map[name] = l;
            }
        }
        
        l.push(f);
    };
    
    var _publish = function(name, data, source) {
        if (_haveName(name))
        {
            if (_map.hasOwnProperty(name))
            {
                var l = _map[name];
                var len = l.length;
                for (var ctr = 0; ctr < len; ++ctr) {
                    l[ctr](data, source);
                }
            }
            else
            {
                // shouldn't happen...
                console.warn("NotificationCenter: key has gone missing: " + name);
            }
        }
    };
    
    return {
        subscribe : _subscribe,
        publish : _publish
    }
};