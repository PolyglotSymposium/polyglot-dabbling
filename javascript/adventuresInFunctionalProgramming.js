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

var array = function (thing) {
    if (!thing && thing !== false) {
        return [];
    }
    else if (thing.constructor === Array) {
        return thing;
    }
    return [thing];
};
