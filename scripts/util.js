NB.Utils = { };
    
NB.Utils.Uuid = function() {
    var _hex = '0123456789abcdef';

    // From: http://blogs.cozi.com/tech/2010/04/generating-uuids-in-javascript.html
    // (c) George V. Reilly
    // BSD Licensed
    var _uuid = function(w1, w2, w3, w4, version) {
        var result = new Array(36);
        var data = [
            (w1 & 0xFFFFFFFF),
            (w2 & 0xFFFF0FFF) | ((version || 4) << 12), // version (1-5)
            (w3 & 0x3FFFFFFF) | 0x80000000,    // rfc 4122 variant
            (w4 & 0xFFFFFFFF)
        ];
        
        for (var i = 0, k = 0; i < 4; ++i)
        {
            var rnd = data[i];
            for (var j = 0; j < 8; ++j)
            {
                if (k === 8 || k === 13 || k === 18 || k ===23) {
                    result[k++] = '-';
                }
                
                var r = (rnd >> 28) & 0xf; // Take the high-order nybble...
                rnd = (rnd & 0x0FFFFFFF) << 4;
                
                result[k++] = _hex.charAt(r);
            }
        }
        
        return result.join('');
    };
    
    // Return a random integer in [0, 2^32).
    // You should include David Bau's seedrandom.js before this script to get
    // better randomness...
    var _randomInt = function()
    {
        return Math.floor(0x100000000 * Math.random());
    };
    
    var _create = function(version) {
        if (!version)
            version = 4;
            
        return _uuid(
            _randomInt(), _randomInt(), _randomInt(), _randomInt(), version
        );
    };
    
    return {
        create: _create
    }
}();