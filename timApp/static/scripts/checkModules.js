var checkModules = function(xs) {
    ys = [];
    for (var i = 0; i < xs.length; i++) {
        try {
            angular.module(xs[i]);
            if (ys.indexOf(xs[i] === -1)) {
                ys.push(xs[i]);
            }
        } catch (err) {
            console.log("Some modules not defined: " + xs[i]);
        }
    }
    return ys;
};