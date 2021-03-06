"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var esSpec_1 = require("./esSpec");
function raise(msg) {
    var wrappedMsg = "<error: " + msg + ">";
    console.error(wrappedMsg);
    return {
        type: "Verbatim",
        verbatim: wrappedMsg
    };
}
function raiseProps(node, expected, actual) {
    return raise("type '" + node + "' expected " + expected + " props, got " + actual);
}
function readWord(inp) {
    inp.str = inp.str.trimLeft();
    var end = inp.str.indexOf(" ");
    if (end == -1) {
        end = inp.str.indexOf("\n");
    }
    if (end == -1) {
        end = inp.str.length;
    }
    var res = inp.str.substring(0, end);
    inp.str = inp.str.substring(end);
    return res;
}
function readInteger(inp) {
    var asStr = readWord(inp);
    var int = Number.parseInt(asStr);
    if (Number.isNaN(int)) {
        return raise("expected an integer, got " + asStr);
    }
    return int;
}
function readFloat(inp) {
    var asStr = readWord(inp);
    var float = Number.parseFloat(asStr);
    if (Number.isNaN(float) && asStr != "NaN") {
        return raise("expected a float, got " + asStr);
    }
    return float;
}
function readString(inp) {
    inp.str = inp.str.trimLeft();
    if (inp.str[0] != '"') {
        return raise("expected a string, got " + inp.str);
    }
    inp.str = inp.str.substring(1);
    var end = 0;
    do {
        end = inp.str.indexOf('"', end);
    } while (end != 0 && inp.str[end - 1] == "\\");
    var res = inp.str.substring(0, end);
    if (end == inp.str.length) {
        return raise('unterminated string: "' + res + '"');
    }
    inp.str = inp.str.substring(end + 1);
    return unescape(res);
}
function readList(inp, spec, lst) {
    var numProps = readInteger(inp);
    if (numProps != 2) {
        return raiseProps("Cons", 2, numProps);
    }
    lst.push(readAst(inp, spec));
    var nextWord = readWord(inp);
    switch (nextWord) {
        case "Nil":
            numProps = readInteger(inp);
            if (numProps != 0) {
                return raiseProps(nextWord, 0, numProps);
            }
            break;
        case "Cons":
            readList(inp, spec, lst);
            break;
    }
    return lst;
}
function readAst(inp, spec) {
    var word = readWord(inp);
    if (typeof word != "string")
        //Error
        return word;
    var numProps;
    switch (word) {
        case "":
            return raise("expected more expressions");
        case "integer":
            return readInteger(inp);
        case "float":
            return readFloat(inp);
        case "string":
            return readString(inp);
        case "splice":
            return {
                type: "Splice",
                index: readInteger(inp)
            };
        case "Unit":
            numProps = readInteger(inp);
            if (numProps != 0) {
                return raiseProps(word, 0, numProps);
            }
            return null;
        case "True":
            numProps = readInteger(inp);
            if (numProps != 0) {
                return raiseProps(word, 0, numProps);
            }
            return true;
        case "False":
            numProps = readInteger(inp);
            if (numProps != 0) {
                return raiseProps(word, 0, numProps);
            }
            return false;
        case "Nil":
            numProps = readInteger(inp);
            if (numProps != 0) {
                return raiseProps(word, 0, numProps);
            }
            return new esSpec_1.ESNodeArray();
        case "Cons":
            var lst = new esSpec_1.ESNodeArray();
            readList(inp, spec, lst);
            return lst;
        case "Undefined":
            numProps = readInteger(inp);
            if (numProps != 0) {
                return raiseProps(word, 0, numProps);
            }
            return undefined;
        case "RegExp":
            numProps = readInteger(inp);
            if (numProps != 1) {
                return raiseProps(word, 1, numProps);
            }
            var nextWord = readWord(inp);
            if (nextWord != "string") {
                return raise("expected regexp to have string property, got word: " + nextWord);
            }
            var regex = readString(inp);
            if (typeof regex != "string")
                //Is error
                return regex;
            return new RegExp(regex);
        case "BigInt":
            numProps = readInteger(inp);
            if (numProps != 1) {
                return raiseProps(word, 1, numProps);
            }
            var nextWord = readWord(inp);
            if (nextWord != "string") {
                return raise("expected bigint to have string property, got word: " + nextWord);
            }
            var bigint = readString(inp);
            if (typeof bigint != "string")
                //Is error
                return bigint;
            return BigInt(bigint);
        default:
            if (word.startsWith(spec.prefix + "_")) {
                var type = word.substring((spec.prefix + "_").length);
                numProps = readInteger(inp);
                var props = spec.nodes[type];
                if (typeof props == "undefined") {
                    return raise("unknown node type: " + type);
                }
                if (numProps != props.length) {
                    return raiseProps(word, props.length, numProps);
                }
                var node = { type: type };
                props.forEach(function (prop) {
                    node[prop] = readAst(inp, spec);
                });
                return node;
            }
            else {
                return raise("unknown word: " + word);
            }
    }
}
function esRead(str, spec) {
    var res = [];
    str.split("\n").forEach(function (line) {
        var inp = { str: line };
        res.push(readAst(inp, spec));
    });
    return res;
}
exports.esRead = esRead;
