exports.requestAnimationFrame = function(eff) {
    function loop() {
        eff()
        window.requestAnimationFrame(loop)
    }

    return loop
}