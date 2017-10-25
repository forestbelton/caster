var keys = {}
var lastKey = null

exports.initKeysImpl = function(unit, id) {
    var el = document.getElementById(id)

    el.addEventListener('keydown', function(ev) {
        keys[ev.keyCode] = ev.keyCode !== lastKey
        lastKey = ev.keyCode
    })

    el.addEventListener('keyup', function(ev) {
        lastKey = null
        delete keys[ev.keyCode]
    })

    return function() {
        return unit
    }
}

exports.checkKey = function(keyCode) {
    return function() {
        var result = keys[keyCode]

        delete keys[keyCode]
        return result === true
    }
}