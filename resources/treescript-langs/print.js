#!/usr/bin/env node --experimental-modules --no-warnings
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var readline_1 = __importDefault(require("readline"));
function raise(msg) {
    return "<error: " + msg + ">";
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
    return readWord(inp);
}
function readFloat(inp) {
    return readWord(inp);
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
function readAst(inp) {
    var word = readWord(inp);
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
            return "\\" + readInteger(inp);
        default:
            if (word.startsWith("StxLisp_Lang_")) {
                var type = word.substring("StxLisp_Lang_".length);
                numProps = parseInt(readInteger(inp));
                var res = [type];
                for (var i = 0; i < numProps; i++) {
                    res.push(readAst(inp));
                }
                return res.join(" ");
            }
            else {
                return raise("unknown word: " + word);
            }
    }
}
function readList(inp) {
    var word = readWord(inp);
    var numProps = parseInt(readInteger(inp));
    switch (word) {
        case "Nil":
            return "";
        case "Cons":
            if (numProps != 2)
                return raise("cons must have 2 elements, got " + numProps);
            return readAst(inp) + readList(inp);
        default:
            return raise("expected list head, got " + word);
    }
}
var rl = readline_1.default.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});
function print() {
    rl.on("line", function (line) {
        var inp = { str: line };
        console.log(readList(inp));
        if (inp.str.trim() != "") {
            var errMsg = "expected less expressions, leftover: " + inp.str;
            console.error(errMsg);
            console.log(errMsg);
        }
    });
}
exports.default = print;
