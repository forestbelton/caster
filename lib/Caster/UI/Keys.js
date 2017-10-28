var initialized = false

var keys = {}
var lastKey = null

exports.initKeysImpl = function(unit) {
    if (initialized) {
        return
    }
    initialized = true

    window.addEventListener('keydown', function(ev) {
        keys[ev.keyCode] = ev.keyCode !== lastKey
        lastKey = ev.keyCode
    })

    window.addEventListener('keyup', function(ev) {
        lastKey = null
        delete keys[ev.keyCode]
    })

    return unit
}

exports.checkKey = function(keyCode) {
    return function() {
        var result = keys[keyCode]

        delete keys[keyCode]
        return result === true
    }
}