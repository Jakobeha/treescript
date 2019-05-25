"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var parse_1 = require("./parse");
exports.parse = parse_1.parse;
function print(inp) {
    return String(inp);
}
exports.print = print;
exports.spec = {
    prefix: "StxLisp_Lang",
    nodes: {
        Word: ["text"],
        Punc: ["punc"],
        Block: ["punc", "content"],
        String: ["punc", "text"],
        Int: ["base", "value"]
    }
};
