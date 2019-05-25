#!/usr/bin/env node --experimental-modules --no-warnings
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var get_stdin_1 = __importDefault(require("get-stdin"));
var grammar_js_1 = __importDefault(require("./grammar.js"));
function parse() {
    get_stdin_1.default().then(function (str) {
        if (str.startsWith("#!")) {
            str = str.substr(str.indexOf("\n"));
        }
        process.stdout.write(grammar_js_1.default.parse(str) + "\n");
    });
}
exports.default = parse;
