"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var get_stdin_1 = __importDefault(require("get-stdin"));
var readline_1 = __importDefault(require("readline"));
var esRead_1 = require("./esRead");
var esWrite_1 = __importDefault(require("./esWrite"));
var grammars_1 = __importDefault(require("./grammars"));
var mode = process.argv[2];
var lang = process.argv[3];
var grammar = grammars_1.default[lang];
if (!grammar) {
    throw "unknown language: " + lang;
}
switch (mode) {
    case "parse":
        parse(grammar);
        break;
    case "print":
        print(grammar);
        break;
    default:
        throw "invalid mode: " + mode;
}
function parse(grammar) {
    get_stdin_1.default().then(function (str) {
        var nodes = grammar.parse(str);
        esWrite_1.default(process.stdout, nodes, grammar.spec);
    });
}
function print(grammar) {
    var rl = readline_1.default.createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false
    });
    rl.on("line", function (line) {
        var nodes = esRead_1.esRead(line, grammar.spec);
        nodes.forEach(function (node) {
            process.stdout.write(grammar.print(node) + "\n");
        });
    });
}
