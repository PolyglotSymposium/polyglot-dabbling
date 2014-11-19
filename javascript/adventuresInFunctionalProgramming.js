var compose = function (f, g) {
    return function (x) {
        return f(g(x));
    };
};

var curry = function (f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
};

var uncurry = function (f) {
    return function (x, y) {
        return f(x)(y);
    };
};
