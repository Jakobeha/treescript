(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.ffi = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
function __export(m) {
    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
}
Object.defineProperty(exports, "__esModule", { value: true });
__export(require("./src/main/assert"));
__export(require("./src/main/convert"));
__export(require("./src/main/ffi"));
__export(require("./src/main/value"));
__export(require("./src/scm"));

},{"./src/main/assert":2,"./src/main/convert":3,"./src/main/ffi":4,"./src/main/value":5,"./src/scm":6}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function assert(pred) {
    if (!pred) {
        throw "assertion failed";
    }
}
exports.default = assert;

},{}],3:[function(require,module,exports){
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var assert_1 = __importDefault(require("./assert"));
var value_1 = require("./value");
Object.prototype.fromValue = function () {
    switch (typeof this) {
        case "number":
        case "string":
            return this;
        case "object":
            if (typeof this.head === "undefined" || typeof this.props === "undefined") {
                throw "not a TreeScript value: " + this.toString();
            }
            var fromValue = fromValues[this.head];
            if (typeof fromValue !== "undefined") {
                return fromValue.apply(this, this.props);
            }
            else {
                return this;
            }
        case "function":
        case "symbol":
        case "undefined":
            throw "not a TreeScript value: " + this.toString();
    }
};
Object.prototype.toValue = function () {
    switch (typeof this) {
        case "bigint":
            return value_1.bigInt(this);
        case "boolean":
            if (this) {
                return value_1.true_;
            }
            else {
                return value_1.false_;
            }
        case "function":
            throw "can't convert function into TreeScript value: " + this.toString();
        case "number":
            return this;
        case "object":
            if (this == null) {
                return value_1.null_;
            }
            else {
                throw "can't convert this object into TreeScript value (override?): " + this.toString();
            }
        case "string":
            return this;
        case "symbol":
            throw "can't convert symbol into TreeScript value: " + this.toString();
        case "undefined":
            throw "can't convert undefined into TreeScript value";
        default:
            throw "can't convert " + typeof this + " into TreeScript value: " + this.toString();
    }
};
Array.prototype.toValue = function () {
    return value_1.list(this);
};
/// Add a function which will convert a TreeScript record into a JS value.
/// The value should have an overridden `toValue` to convert it back.
/// These converters will be used implicitly when passing values to/from the program.
function addFromValue(head, f) {
    fromValues[head] = f;
}
exports.addFromValue = addFromValue;
var fromValues = {};
addFromValue("Nil", function () { return []; });
addFromValue("Cons", function (head, tail) {
    var res = tail.fromValue();
    assert_1.default(Array.isArray(res));
    res.unshift(head.fromValue());
    return res;
});

},{"./assert":2,"./value":5}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/// Call a JS library function with TreeScript values, converts the values into JS going into the function and back to TreeScript going out.
function callFfi(f) {
    var args = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        args[_i - 1] = arguments[_i];
    }
    var out = f.apply(void 0, (args.map(function (arg) { return arg.fromValue(); }))).toValue();
    console.log(JSON.stringify(out));
}
exports.callFfi = callFfi;

},{}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function record(head) {
    var props = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        props[_i - 1] = arguments[_i];
    }
    return {
        head: head,
        props: props,
    };
}
exports.record = record;
function bigInt(x) {
    return record("BigInt", x.toString());
}
exports.bigInt = bigInt;
function list(x, nilHead, consHead) {
    if (nilHead === void 0) { nilHead = "Nil"; }
    if (consHead === void 0) { consHead = "Cons"; }
    var res = record(nilHead);
    var rem = new (Array.bind.apply(Array, [void 0].concat(x)))();
    while (rem.length > 0) {
        res = record(consHead, rem.pop().toValue(), res);
    }
    return res;
}
exports.list = list;
exports.true_ = record("True");
exports.false_ = record("False");
exports.null_ = record("Null");

},{}],6:[function(require,module,exports){
"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var assert_1 = __importDefault(require("./main/assert"));
var convert_1 = require("./main/convert");
var value_1 = require("./main/value");
var SList = /** @class */ (function (_super) {
    __extends(SList, _super);
    function SList() {
        var items = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            items[_i] = arguments[_i];
        }
        return _super.apply(this, items) || this;
    }
    SList.prototype.toValue = function () {
        return value_1.list(this, "SNil", "SCons");
    };
    return SList;
}(Array));
exports.SList = SList;
convert_1.addFromValue("Scheme_Lang_SNil", function () { return new SList([]); });
convert_1.addFromValue("Scheme_Lang_SCons", function (head, tail) {
    var res = tail.fromValue();
    assert_1.default(res instanceof SList);
    res.unshift(head.fromValue());
    return res;
});

},{"./main/assert":2,"./main/convert":3,"./main/value":5}]},{},[1])(1)
});
